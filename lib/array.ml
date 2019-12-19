open import "amulet/exception.ml"
open import "prelude.ml"

private type storage 'a

type array 'a =
  private Array of {
    length  : int,
    offset  : int,
    backing : storage 'a
  }

let private in_bounds str (Array r) i =
  if (r.offset + i <= r.length) && (i >= 1) then
    ()
  else
    throw (Invalid (str ^ ": index " ^ show i ^ " is out of bounds"))

external private val tabulate : int -> (int -> 'a) -> storage 'a =
  "function(index, cont) \
     local r = {} \
     for i = 1, index do \
       r[i] = cont(i) \
     end \
     return r \
   end"

external private val geti : storage 'a -> int -> 'a = "rawget"
external private val seti : storage 'a -> int -> 'a -> storage 'a = "rawset"

let make len arg =
  if len < 0 then
    throw (Invalid ("init: can't initialise array of size " ^ show len))
  else ()
  Array {
    length = len,
    offset = 0,
    backing = tabulate len (fun _ -> arg)
  }

let init len cont =
  if len < 0 then
    throw (Invalid ("init: can't initialise array of size " ^ show len))
  else ()
  Array {
    length = len,
    offset = 0,
    backing = tabulate len cont
  }

let copy (Array r) =
  init (r.length - r.offset) @@ fun i ->
    geti r.backing (i + r.offset)

let from_list (li : list 'a) =
  let storage = tabulate 0 (fun _ -> (error "") : 'a)
  let rec loop i = function
    | [] -> i
    | Cons (x, xs) ->
        let _ = seti storage i x
        loop (i + 1) xs
  let len = loop 1 li
  Array {
    length = len - 1,
    offset = 0,
    backing = storage
  }

let range len = init len (fun i -> i)

instance functor array begin
  let f <$> (Array r) = Array {
    length  = r.length,
    offset  = r.offset,
    backing = tabulate r.length (fun i ->
      f (geti r.backing (i + r.offset)))
  }
end

instance foldable array begin
  let foldr func acc (Array r) =
    let rec loop acc i =
      if i < 1 + r.offset then
        acc
      else
        loop (func (geti r.backing i) acc) (i - 1)
    loop acc r.length

  let foldl func acc (Array r) =
    let rec loop acc i =
      if i > r.length then
        acc
      else
        loop (func acc (geti r.backing i)) (i + 1)
    loop acc r.offset
end

instance show 'a => show (array 'a) begin
  let show arr = "to_list " ^ show (to_list arr)
end

instance eq 'a => eq (array 'a) begin
  let (Array a) == (Array b) =
    let elem_cmp (x : 'a) (y : 'a) = x == y
    (a.length - a.offset) == (b.length - b.offset) &&
      let rec loop (i : int) : bool =
          if i > a.length then
            true
          else
            geti a.backing i `elem_cmp` geti b.backing i || loop (i + 1)
      loop a.offset
end

let ( .() ) (Array r as arr) i =
  in_bounds "(.())" arr i
  geti r.backing (i + r.offset)

let ( .()<- ) (Array r as arr) i x =
  in_bounds "update" arr i
  let _ = seti r.backing (i + r.offset) x
  ()

let take n (Array r) =
  if r.length - n >= r.offset then
    Array {
      length = n,
      offset = r.offset,
      backing = r.backing
    }
  else
    throw (Invalid ("take: can't take " ^ show n ^ " elements"))

let drop n (Array r) =
  if r.offset + n <= r.length then
    Array {
      length = r.length,
      offset = r.offset + n,
      backing = r.backing
    }
  else
    throw (Invalid ("drop: can't drop " ^ show n ^ " elements"))

let append (Array ra) (Array rb) =
  Array {
    length = (ra.length - ra.offset) + (rb.length - rb.offset),
    offset = 0,
    backing = tabulate (ra.length + rb.length) (fun i ->
      print i
      if i > ra.length then
        geti rb.backing (i - ra.length + rb.offset)
      else
        geti ra.backing (i + ra.offset))
  }
