type list 'a =
  | Nil  : list 'a
  | Cons : 'a * list 'a -> list 'a

type elem 'x 'xs =
  | Here : elem 'x (Cons ('x * 'xs))
  | There : elem 'x 'xs -> elem 'x (Cons ('y * 'xs))

type product 'a =
  | Unit : product Nil
  | Pair : 'a * product 'b -> product (Cons ('a * 'b))

let a :: b = Pair (a, b)

let foo = 1 :: () :: "string" :: false :: 1.0 :: Unit
