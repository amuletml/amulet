let main =
  let id x = x
  let (foo, _) = (id, 1)
  foo 1
  foo ()
  foo ""
