let main =
  let id x = x
  let cross f g (x, y) = (f x, g y)
  let second x = cross id x
  let _ = second (+ 1) (1, 2)
  second (^ "bar") (1, "foo")
