let main =
  let id x = x
  let (foo, _) = (id, 1)
  let _ = foo 1
  let _ = foo ()
  foo ""
