type foo 'a =
  | Foo : foo bool

let bar : forall 'a. 'a * foo 'a -> int = function
  | (x, Foo) when x -> 1
