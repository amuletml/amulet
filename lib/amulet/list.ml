module Amc = import "amulet/prim.ml"
open import "./base.ml"
open import "./exception.ml"

let reverse xs =
  let rec loop acc = function
    | [] -> acc
    | Cons (x, xs) -> loop (x :: acc) xs
  loop [] xs

let filter p xs =
  let rec filter_acc acc = function
    | [] -> acc
    | Cons (x, xs) ->
        if p x then
          filter_acc (x :: acc) xs
        else
          filter_acc acc xs
  reverse (filter_acc [] xs)

let rec private aux ys prev = function
  | [] -> Amc.set_snd prev ys
  | Cons (x, xs) ->
      let next = Amc.alloc_tuple x []
      Amc.set_snd prev (Cons (Amc.from_repr next))
      aux ys next xs

let x ++ y =
  match x, y with
  | x, [] -> x
  | [], y -> y
  | Cons (x, xs), ys ->
      let next = Amc.alloc_tuple x []
      aux ys next xs
      Cons (Amc.from_repr next)

let concat = join @list

let rec zip_with k xs ys =
  match xs, ys with
  | Cons (x, xs), Cons (y, ys) -> k x y :: zip_with k xs ys
  | _, _ -> []

let zip xs = zip_with (,) xs

let mapi cont =
  let rec loop i = function
    | [] -> []
    | Cons (x, xs) -> Cons (cont i x, loop (i + 1) xs)
  loop 0

let rec iter cont = function
  | [] -> ()
  | Cons (x, xs) ->
      cont x
      iter cont xs

let iteri cont =
  let rec loop i = function
    | [] -> ()
    | Cons (x, xs) ->
        cont x
        loop (i + 1) xs
  loop 0

let head xs =
  match xs with
  | [] -> throw (Invalid "head")
  | Cons (x, _) -> x

let tail xs =
  match xs with
  | [] -> throw (Invalid "tail")
  | Cons (_, xs) -> xs

let head_or x xs =
  match xs with
  | [] -> x
  | Cons (x, _) -> x

let tail_or ys xs =
  match xs with
  | [] -> ys
  | Cons (_, xs) -> xs
