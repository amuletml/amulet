type exp

type ast 'a 'b =
  | Var : string -> ast exp 'tag
  | Tag : 'tag * ast 'a 'tag -> ast 'a 'tag

type foo = Foo of int

let rec convert : forall 'a 'tag. ast 'a 'tag -> ast 'a foo = function
  | Var v -> Tag (Foo 42, Var v)
  | Tag (t,e) -> Tag (Foo 42, convert e)
