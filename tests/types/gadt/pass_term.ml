type list 'tys =
  | Nil : list ()
  | Cons : 'a * list 'b -> list ('a * 'b)

type elem 'x 'xs =
  | Here : elem 'x ('x * 'xs)
  | There : elem 'x 'xs -> elem 'x ('y * 'xs)

type term 'ctx 'ty =
  | Var : elem 'ty 'ctx -> term 'ctx 'ty
  | Lam : term ('a * 'ctx) 'b -> term 'ctx ('a -> 'b)
  | App : term 'ctx ('a -> 'b) * term 'ctx 'a -> term 'ctx 'b

let const = Lam (Lam (Var (There Here)))
