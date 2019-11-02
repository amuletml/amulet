type list 'a = Nil | Cons of 'a * list 'a

let map f = function
| [] -> []
| Cons (x, xs) -> Cons (f x, map f xs)

let map' f =
  let go = function
  | [] -> []
  | Cons (x, xs) -> Cons (f x, go xs)
  go
