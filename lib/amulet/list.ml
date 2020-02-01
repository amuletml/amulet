open import "./exception.ml"
open import "./option.ml"
open import "./base.ml"

(** Reverse the order of a list.
 * This function is tail recursive, and does not risk overflowing the
 * stack.
 *
 * Runtime: O(n) *)
let reverse xs =
  let rec loop acc = function
    | [] -> acc
    | Cons (x, xs) -> loop (x :: acc) xs
  loop [] xs

(** Filter a list according to a predicate. Elements for which the
 * predicate returns 'true' will be present in the output list.
 *
 * This function is tail recursive, and does not risk overflowing the
 * stack.
 *
 * Runtime: O(n) *)
let filter p xs =
  let rec filter_acc acc = function
    | [] -> acc
    | Cons (x, xs) ->
        if p x then
          filter_acc (x :: acc) xs
        else
          filter_acc acc xs
  reverse (filter_acc [] xs)

(** Concatenate two lists.
 *
 * This function is *NOT* tail recursive; If the first list is too long,
 * it will overflow the stack!
 *
 * Runtime: O(n) *)
let rec xs ++ ys =
  match xs, ys with
  | [], ys -> ys
  | Cons (x, xs), ys -> Cons (x, xs ++ ys)

(** Do all elements of the list satisfy the predicate p?
 *
 * This function is tail recursive, and does not risk overflowing the
 * stack.
 *
 * Runtime: O(n) *)
let rec all p = function
  | [] -> true
  | Cons (x, xs) -> p x && all p xs

(** Does any element in this list satisfy the predicate p?
 *
 * This function is tail recursive, and does not risk overflowing the
 * stack.
 *
 * Runtime: O(n) *)
let rec any p = function
  | [] -> false
  | Cons (x, xs) -> p x || any p xs

(** Concatenate a list of lists.
 *
 * This function is *NOT* tail recursive; If any of the lists is too
 * long, it will overflow the stack!
 *
 * Runtime: O(sum(lengths)) *)
let concat = join @list

(** Produce a single list by joining elements in matching positions from
 * the two input lists with the function k.
 *
 * This function is *NOT* tail recursive; If the first list is too long,
 * it will overflow the stack!
 *
 * Runtime: O(min(length xs, length ys)) *)
let rec zip_with k xs ys =
  match xs, ys with
  | Cons (x, xs), Cons (y, ys) -> k x y :: zip_with k xs ys
  | _, _ -> []

(** Produce a single list by joining elements in matching positions from
 * the two input lists into a tuple.
 *
 * This function is *NOT* tail recursive; If the first list is too long,
 * it will overflow the stack!
 *
 * Runtime: O(min(length xs, length ys)) *)
let zip xs = zip_with (,) xs

(** Map a function over the input list, passing the position of the
 * element as well as the element.
 *
 * This function is *NOT* tail recursive; If the first list is too long,
 * it will overflow the stack!
 *
 * Runtime: O(min(length xs, length ys)) *)
let imap cont =
  let rec loop i = function
    | [] -> []
    | Cons (x, xs) -> Cons (cont i x, loop (i + 1) xs)
  loop 0

(** Run an (effectful) function over a list and discard all the unit
 * results.
 *
 * This function is tail recursive, and does not risk overflowing the
 * stack.
 *
 * Runtime: O(n) *)
let rec iter cont = function
  | [] -> ()
  | Cons (x, xs) ->
      cont x
      iter cont xs

(** Run an (effectful) function over a list and discard all the unit
 * results, passing the index of the element together with the element.
 *
 * This function is tail recursive, and does not risk overflowing the
 * stack.
 *
 * Runtime: O(n) *)
let iiter cont =
  let rec loop i = function
    | [] -> ()
    | Cons (x, xs) ->
        cont x
        loop (i + 1) xs
  loop 0

(** Fold a list, passing the index together with the current element and
 * the accumulator.
 *
 * This function is tail recursive, and does not risk overflowing the
 * stack.
 *
 * Runtime: O(n) *)
let ifoldl f z =
  let rec loop z i = function
    | [] -> z
    | Cons (x, xs) -> loop (f i x z) (i + 1) xs
  loop z 0

(** Retrieve the i-th element of a list.
 *
 * This function is tail recursive, and does not risk overflowing the
 * stack.
 *
 * Runtime: O(min(length xs, i)) *)
let rec nth i xs =
  match i, xs with
  | k, _ when k < 0 -> throw (Invalid ("nth: negative index"))
  | 0, Cons (x, _)  -> x
  | n, Cons (_, xs) -> nth (n - 1) xs
  | _, [] -> throw (Invalid ("nth: index out of range"))

(** Get the first element of a list.
 * Runtime: O(1) *)
let head xs =
  match xs with
  | [] -> throw (Invalid "head")
  | Cons (x, _) -> x

(** Get all but the first element of a list.
 * Runtime: O(1) *)
let tail xs =
  match xs with
  | [] -> throw (Invalid "tail")
  | Cons (_, xs) -> xs

(** Get the first element of a list, returning x if the list is empty.
 * Runtime: O(1) *)
let head_or x xs =
  match xs with
  | [] -> x
  | Cons (x, _) -> x

(** Get all but the first element of a list, returning x if the list is empty.
 * Runtime: O(1) *)
let tail_or ys xs =
  match xs with
  | [] -> ys
  | Cons (_, xs) -> xs

let rec private sequences cmp ass =
  match ass with
  | Cons (a, Cons (b, ass)) ->
      if a `cmp` b == Gt then
        descending cmp b [a] ass
      else
        ascending cmp b (a::) ass
  | ass -> [ass]
and descending cmp a ass ys =
  match ys with
  | Cons (y, ys) ->
      if a `cmp` y == Gt then
        descending cmp y (a :: ass) ys
      else
        (a :: ass) :: sequences cmp (y :: ys)
  | _ -> (a :: ass) :: sequences cmp ys
and ascending cmp a k bs =
  match bs with
  | Cons (b, bs) ->
      if (a `cmp` b) <> Gt then
        ascending cmp b (fun ys -> k (a :: ys)) bs
      else
        k [a] :: sequences cmp (b :: bs)
  | _ -> k [a] :: sequences cmp bs

let rec private merge_all cmp = function
  | [x] -> x
  | xs -> merge_all cmp (merge_pairs cmp xs)
and merge_pairs cmp = function
  | Cons (a, Cons (b, xs)) -> merge cmp (a, b) :: merge_pairs cmp xs
  | xs -> xs
and merge cmp = function
  | [], ys -> ys
  | xs, [] -> xs
  | Cons (x, xs') as xs, Cons (y, ys') as ys ->
      match x `cmp` y with
      | Gt -> y :: merge cmp (xs, ys')
      | _  -> x :: merge cmp (xs', ys)

(** Sort a list using the given comparison function.
 *
 * Runtime: O(n log n)
 *)
let sort_by cmp xs = merge_all cmp (sequences cmp xs)

(** Sort a list using the polymorphic compare function, from the ord
 * typeclass.
 *
 * Runtime: O(n log n)
 *)
let sort xs = sort_by compare xs

(** Find an element in a list of key-value pairs.
 *
 * Runtime: O(n)
 *)
let rec lookup key = function
  | Cons ((k, v), xs) ->
      if k == key then
        Some v
      else
        lookup key xs
  | [] -> None

(** 'scanl' is similar to 'foldl', but it returns a list containing
 * successive results of the stepper function.
 *
 * Runtime: O(n)
 *)

let scanl func q ls =
  let rec go k z = function
    | [] -> k [z]
    | Cons (x, xs) ->
        go (fun xs -> k (z :: xs)) (func z x) xs
  go (fun x -> x) q ls

(**
 * compact takes a list of optionals and returns the present elements
 *
 * Runtime: O(n)
 *)
let rec compact = function
  | Nil -> []
  | Cons (None, tl) -> compact tl
  | Cons (Some a, tl) -> a :: compact tl

(**
 * length returns the number of elements in the list
 *
 * Runtime: O(n)
 *)
let rec length = function
  | Nil -> 0
  | Cons (_, tl) -> 1 + length tl

