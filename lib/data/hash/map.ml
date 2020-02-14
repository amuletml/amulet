open import "../../prelude.ml"
open import "../../lua/conversion.ml"
open import "../hash.ml"
include import "../index.ml"

private module Math = import "../../lua/math.ml"
private module Array = import "../array.ml"

private type pairs 'k 'v =
  | Nil
  | Cons of { k : 'k, v : 'v, next : pairs 'k 'v }

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

instance hashable 'k => index (t 'k 'v)
  type key = 'k
  type value = 'v

  let ( .?() ) (HashMap container) k =
    let Container { array, size } = !container
    if size == 0 then None else
    let rec find = function
      | Nil -> None
      | Cons { k = k', v } when k == k' -> Some v
      | Cons { next } -> find next
    let h = (hash k .&. (Array.size array - 1)) + 1
    find (array.[h])

let iter f (HashMap container) =
  let Container { array } = !container
  let rec iter_one = function
  | Nil -> ()
  | Cons { k, v, next } -> f k v;iter_one next
  Array.iter iter_one array

let private put_direct array k v =
  (* This is kind of terrible - we entirely tear down the old list and
     add a new one. But it's tail recursive, so that's nice I guess... *)
  let rec find add old = function
    | Nil -> (add, Cons { k, v, next = old })
    | Cons { k = k', next } when k' == k -> find false old next
    | Cons { k, v, next} -> find add (Cons { k, v, next = old }) next
  let h = (hash k .&. (Array.size array - 1)) + 1
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

instance hashable 'k => mut_index (t 'k 'v)
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
