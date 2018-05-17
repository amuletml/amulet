let main f =
  match f () with
  | { a = a, b = b } -> a + b
