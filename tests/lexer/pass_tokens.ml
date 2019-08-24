( (* Numbers *)
  2 200 0xf20a 0b1101
  2.0 2.0e5 2.0e-5

  (* Strings *)
  "" "x" "\a" "\"" "\n\\\"\n" "\x23"
  "\
  Hello"

  (* Identifiers *)
  foo Foo
  Foo.x Foo.X Foo.X.Y.z
  Foo.X.y.z

  `foo`
  `X.foo`
  _foo
  'foo
  .foo
)
