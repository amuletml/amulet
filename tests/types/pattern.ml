type foo = Foo of int * int * int
let _ : int * int =
  let Foo (a, b) = Foo (1, 2, 3)
  b
