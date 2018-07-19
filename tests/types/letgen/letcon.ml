type foo 'a =
  Foo of 'a

let main =
  let foo x = (x, Foo x)
  foo 1
  foo ()
