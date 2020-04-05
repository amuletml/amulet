open import "../amulet/exception.ml"
open import "../prelude.ml"
include import "./index.ml"

private type storage 'a

type array 'a =
  private Array of {
    length  : int,
    backing : storage 'a
  }

let private is_in_bounds i (Array { length }) =
  (* Flip the argument order so the pattern match happens last. Otherwise this
  prevents uncurrying. *)
  i >= 0 && i < length

let private in_bounds str r i =
  if is_in_bounds i r then () else
  throw (Invalid (str ^ ": index " ^ show i ^ " is out of bounds"))

external private val tabulate : int -> (int -> 'a) -> storage 'a =
  "function(index, cont) \
     local r = {} \
     for i = 1, index do \
       r[i] = cont(i) \
     end \
     return r \
   end"

external private val empty_storage : storage 'a = "{}"
external private val geti : storage 'a -> int -> 'a = "rawget"
external private val seti : storage 'a -> int -> 'a -> storage 'a = "rawset"

let make len arg =
  if len < 0 then
    throw (Invalid ("init: can't initialise array of size " ^ show len))
  else ()
  Array {
    length = len,
    backing = tabulate len (fun _ -> arg)
  }

let size (Array { length }) = length

let init len gen =
  if len < 0 then
    throw (Invalid ("init: can't initialise array of size " ^ show len))
  else ()
  Array {
    length = len,
    backing = tabulate len (fun i -> gen (i - 1))
  }

let copy (Array { length, backing }) =
  Array { length, backing = tabulate length (geti backing) }

let from_list (li : list 'a) =
  let backing = tabulate 0 (fun _ -> (error "") : 'a)
  let rec loop i = function
    | [] -> i
    | Cons (x, xs) ->
        let _ = seti backing (i + 1) x
        loop (i + 1) xs
  let length = loop 0 li
  Array { length, backing }

let range len = init len (fun i -> i)

instance functor array begin
  let f <$> (Array { length, backing }) = Array {
    length,
    backing = tabulate length (fun i -> f (geti backing i))
  }
end

instance foldable array begin
  let foldr func acc (Array { length, backing }) =
    let rec loop acc i =
      if i < 1 then
        acc
      else
        loop (func (geti backing i) acc) (i - 1)
    loop acc length

  let foldl func acc (Array { length, backing }) =
    let rec loop acc i =
      if i > length then
        acc
      else
        loop (func acc (geti backing i)) (i + 1)
    loop acc 1
end

instance show 'a => show (array 'a) begin
  let show arr = "to_list " ^ show (to_list arr)
end


instance eq 'a => eq (array 'a) begin
  let Array a == Array b =
    let elem_cmp (x : 'a) (y : 'a) = x == y
    a.length == b.length &&
      let rec loop (i : int) : bool =
        if i > a.length then
          true
        else
          geti a.backing i `elem_cmp` geti b.backing i && loop (i + 1)
      loop 1
end

let empty = Array { length = 0, offset = 0, backing = empty_storage }

let iter f (x : array 'a) = foldl (fun () -> f) () x

let iteri f (x : array 'a) = foldl (fun i x -> f i x; i + 1) 0 x

instance index (array 'a) begin
  type key = int
  type value = 'a

  let ( .() ) arr i =
    in_bounds "(.())" arr i
    let Array { backing }  = arr
    geti backing (i + 1)

  let ( .?() ) arr i =
    if is_in_bounds i arr then
      let Array { backing }  = arr
      geti backing (i + 1) |> Some
    else None
end

instance mut_index (array 'a) begin
  let ( .[]<- ) arr i x =
    in_bounds "update" arr i
    let Array { backing }  = arr
    let _ = seti backing (i + 1) x
    ()
end

let append (Array ra) (Array rb) =
  let length = ra.length + rb.length
  Array {
    length,
    backing = tabulate length (fun i ->
      if i > ra.length then
        geti rb.backing (i - ra.length)
      else
        geti ra.backing i)
  }
