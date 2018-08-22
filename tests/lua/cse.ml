external val print : 'a -> unit = "print"
type foo = Foo of int

let use_foo x y =
  print x
  print y
  ()

let () =
  let x = Foo 1
  let y = Foo 1
  use_foo x y
