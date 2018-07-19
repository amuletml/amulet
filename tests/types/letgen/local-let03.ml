let main =
  let id x = x
  let cross f g (x, y) = (f x, g y)
  let second = cross id
  second (+ 1) (1, 2)
  second (^ "bar") (1, "foo")
