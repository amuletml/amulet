open import "../../prelude.ml"
open import "../hash.ml"
include import "../index.ml"

private module Math = import "../../lua/math.ml"
private module Array = import "../array.ml"

private type pairs 'k 'v =
  | Nil
  | Cons of { k : 'k, v : 'v, next : pairs 'k 'v }

instance functor (pairs 'k) begin
  let f <$> x =
    let rec go = function
      | Nil -> Nil
      | Cons { k, v, next } -> Cons { k, v = f v, next = go next }
    go x
end

let rec private pair_fold_with_key f acc = function
  | Nil -> acc
  | Cons { k, v, next } -> pair_fold_with_key f (f k v acc) next

instance foldable (pairs 'k) begin
  let foldl f =
    let rec loop acc = function
      | Nil -> acc
      | Cons { v, next } -> foldl f (f acc v) next
   loop

  let foldr f = foldl (fun acc x -> f x acc)
end

private type container 'k 'v = Container of {
  array : Array.array (pairs 'k 'v),
  size : int,
  initial : int,
}

type t 'k 'v = private HashMap of ref (container 'k 'v)

let load_factor = 2

let make () = HashMap (ref (Container { array = Array.empty, size = 0, initial = 16 }))

let make_with_capacity c =
  let initial = if c <= 1 then 1 else
    Math.log (float_of_int c) /. Math.log 2.0 |> Math.ceil |> (2**)
  HashMap (ref (Container { array = Array.empty, size = 0, initial }))

instance hashable 'k => index (t 'k 'v) begin
  type key = 'k
  type value = 'v

  let ( .?() ) (HashMap container) k =
    let Container { array, size } = !container
    if size == 0 then None else
    let rec find = function
      | Nil -> None
      | Cons { k = k', v } when k == k' -> Some v
      | Cons { next } -> find next
    let h = (hash k .&. (Array.size array - 1))
    find (array.[h])
end

let iter f (HashMap container) =
  let Container { array } = !container
  let rec iter_one = function
    | Nil -> ()
    | Cons { k, v, next } -> f k v; iter_one next
  Array.iter iter_one array

let private put_direct array k v =
  (* This is kind of terrible - we entirely tear down the old list and
     add a new one. But it's tail recursive, so that's nice I guess... *)
  let rec find add old = function
    | Nil -> (add, Cons { k, v, next = old })
    | Cons { k = k', next } when k' == k -> find false old next
    | Cons { k, v, next} -> find add (Cons { k, v, next = old }) next
  let h = (hash k .&. (Array.size array - 1))
  let (added, a') = find true Nil (array.[h])
  array.[h] <- a'
  added

let private ignore _ = ()

let private rehash (HashMap container as hm) size =
  let Container { initial } = !container
  let array = Array.make size Nil
  iter (fun k v -> put_direct array k v |> ignore) hm
  container := Container { array, size, initial }
  array

instance hashable 'k => mut_index (t 'k 'v) begin
  let ( .[]<- ) (HashMap container as hm) k v =
    let Container { array, size, initial } = !container
    let a_size = Array.size array in
    let array =
      if a_size == 0 then
        let array = Array.make initial Nil
        container := Container { array, size, initial }
        array
      else if size + 1 > a_size * 2 then rehash hm (a_size * 2)
      else array
    let added = put_direct array k v
    if added then container := Container { array, size = size + 1, initial } else ()
end

instance functor (t 'k) begin
  let f <$> (HashMap x) =
    let Container { array, size, initial } = !x in
    Container { array = (f <$>) <$> array, size, initial } |> ref |> HashMap
end

instance foldable (t 'k) begin
  let foldl f acc (HashMap x) =
    let Container { array } = !x in
    foldl (foldl f) acc array

  let foldr f = foldl (fun acc x -> f x acc)
end

instance hashable 'k => traversable (t 'k) begin
  let traverse f (HashMap x) =
    (* Traversing over arrays is tricky, so we just reallocate a new map and
       re-insert into that. *)
    let Container { array, size, initial } = !x in
    let new_array = Array.make (Array.size array) Nil in

    let add k v res  = put_direct new_array k v |> ignore; res
    let combine k v res = add k <$> f v <*> res
    let res = HashMap (ref (Container { array = new_array, size, initial }))
    foldl (pair_fold_with_key combine) (| res |) array
end
