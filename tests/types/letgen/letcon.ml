type foo 'a =
  Foo of 'a

let main =
  let foo x = (x, Foo x)
  let _ = foo 1
  foo ()
