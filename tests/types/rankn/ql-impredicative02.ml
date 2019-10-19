let x :: xs = Cons (x, xs)

let ids : list (forall 'a. 'a -> 'a) =
  let empty : list (forall 'a. 'a -> 'a) = []
  (fun x -> x) :: empty

external val (+) : int -> int -> int = ""

let length = function
  | [] -> 0
  | Cons (_, xs) -> 1 + length xs

let x = length ids
