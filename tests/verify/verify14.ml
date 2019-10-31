let rec f x =
  let k = g x
  k
and g x =
  let y = f x
  0
