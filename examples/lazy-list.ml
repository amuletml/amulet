open import "prelude.ml"
open import "amulet/exception.ml"

type list 'a =
  | Nil
  | Cons of 'a * lazy (list 'a)

let a :: b = Cons (a, b)

let rec zip_with f (a : list _) (b : list _) : list _ =
  match a, b with
  | Cons (x, xs), Cons (y, ys) -> f x y :: zip_with f xs ys
  | _ -> Nil

let tail = function
  | Nil -> Exception.(Invalid "Empty list" |> throw)
  | (Cons (_, xs)) -> xs

let rec take n (xs : list 'a) : list 'a =
  match xs with
  | Nil -> Nil
  | Cons (x, xs) ->
     if n <= 0
     then Nil
     else x :: take (n - 1) xs

instance show 'a => show (list 'a)
  let show = function
  | Nil -> "[]"
  | Cons (x, xs) ->
     match force xs with
     | Nil -> "[" ^ show x ^ "]"
     | _ -> let rec go = function
              | Nil -> "]"
              | Cons (x, xs) -> ", " ^ show x ^ go (force xs)
            "[" ^ show x ^ go xs

let rec fibs = 0 :: 1 :: zip_with (+) fibs (tail fibs)

let () = take 20 fibs |> print
