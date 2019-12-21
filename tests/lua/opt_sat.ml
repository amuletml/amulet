let rec map f = function
  | [] -> []
  | Cons (x, xs) -> Cons (f x, map f xs)
