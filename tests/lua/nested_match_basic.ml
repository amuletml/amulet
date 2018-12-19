let a :: b = Cons (a, b)

let zip f xs ys =
  match xs, ys with
  | Nil, _ -> [1]
  | _, Nil -> [2]
  | Cons (x, xs), Cons (y, ys) -> f x y :: zip f xs ys

external val bottom : 'a = "nil"
let () = bottom zip
