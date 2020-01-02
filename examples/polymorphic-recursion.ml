open import "prelude.ml"

type nested 'a = Epsilon | Sub of 'a * nested (list 'a)

let rec length (x : nested 'a) : int =
  match x with
  | Epsilon -> 0
  | Sub (_, xs) -> 1 + length xs

let nested = Sub (1, Sub (Cons (1, Nil), Epsilon))

let () = print (length nested)
