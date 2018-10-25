type list 'a = Nil | Cons of 'a * list 'a

let a :: b = Cons (a, b)

let filter f = function
  | Nil -> Nil
  | Cons (x, xs) when f x -> x :: filter f xs
  | Cons (_, xss) -> filter f xss

external val bottom : 'a -> () = "nil"
let () = bottom filter
