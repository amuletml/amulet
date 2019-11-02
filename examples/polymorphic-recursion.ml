external val (+) : int -> int -> int = ""

type list 'a = Nil | Cons of 'a * list 'a
type nested 'a = Epsilon | Sub of 'a * nested (list 'a)

let rec length (x : nested 'a) : int =
  match x with
  | Epsilon -> 0
  | Sub (_, xs) -> 1 + length xs

let nested = Sub (1, Sub (Cons (1, Nil), Epsilon))

external val print : forall 'a. 'a -> unit = "print"

let () = print (length nested)
