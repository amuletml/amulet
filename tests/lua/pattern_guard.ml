let a :: b = Cons (a, b)

let rec filter f = function
  | Nil -> Nil
  | Cons (x, xs) when f x -> x :: filter f xs
  | Cons (_, xss) -> filter f xss

external val ignore : 'a -> () = "nil"
let () = ignore filter
