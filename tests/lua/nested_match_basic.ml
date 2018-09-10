type list 'a = Nil | Cons of 'a * list 'a

let a :: b = Cons (a, b)

let zip f xs ys =
  match xs, ys with
  | Nil, _ -> 1 :: Nil
  | _, Nil -> 2 :: Nil
  | Cons (x, xs), Cons (y, ys) -> f x y :: zip f xs ys

external val bottom : 'a = "nil"
let () = bottom zip
