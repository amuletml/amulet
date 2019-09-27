type foo 'a =
  | Foo1 : foo int

let bar : forall 'a. 'a ~ int => 'a -> int =
  fun x -> x + 1

let foo : forall 'a. foo 'a -> 'a -> int = fun foo1 x ->
  match foo1 with
  | Foo1 -> bar x
