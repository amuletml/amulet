type list 'tys =
  | Nil : list ()
  | Cons : 'a * list 'b -> list ('a * 'b)

type elem 'x 'xs =
  | Here : elem 'x ('x * 'xs)
  | There : elem 'x 'xs -> elem 'x ('y * 'xs)

let index (x : elem 'a 'ts) (xs : list 'ts) : 'a =
  match x, xs with
  | Here, Cons (a, _) -> a
  | There p, Cons (_, xs) -> index p xs

let (::) a b = Cons (a, b)

let foo = index (There (There Here)) (1 :: () :: "foo" :: Nil)
