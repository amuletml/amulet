let main f =
  match f () with
  | { a = a, b = b } -> a + b

external val bottom : 'a = "nil"
let () = bottom (main bottom)
