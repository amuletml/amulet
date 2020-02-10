external val (+) : int -> int -> int = "function(x, y) return x + y end"

type sz_tree 'a =
  | E
  | T of 'a * int * sz_tree 'a * sz_tree 'a

let rec foldr f zero = function
  | E -> zero
  | T (x, _, l, r) -> foldr f (f x (foldr f zero r)) l

let _ = foldr (+) 0 E
