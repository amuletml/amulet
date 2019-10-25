module Amc = import "amulet/prim.ml"
open import "./base.ml"

let reverse xs =
  let loop acc = function
    | [] -> acc
    | Cons (x, xs) -> loop (x :: acc) xs
  loop [] xs

let filter p xs =
  let filter_acc acc = function
    | [] -> acc
    | Cons (x, xs) ->
        if p x then
          filter_acc (x :: acc) xs
        else
          filter_acc acc xs
  reverse (filter_acc [] xs)

let private aux ys prev = function
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

let zip_with k xs ys =
  match xs, ys with
  | Cons (x, xs), Cons (y, ys) -> k x y :: zip_with k xs ys
  | _, _ -> []

let zip xs = zip_with (,) xs

let mapi cont =
  let loop i = function
    | [] -> []
    | Cons (x, xs) -> Cons (cont i x, loop (i + 1) xs)
  loop 0

let iter cont = function
  | [] -> ()
  | Cons (x, xs) ->
      cont x
      iter cont xs

let iteri cont =
  let loop i = function
    | [] -> ()
    | Cons (x, xs) ->
        cont x
        loop (i + 1) xs
  loop 0
