let main f =
  match f () with
  | { a = a, b = b } -> a + b

external val ignore : 'a -> () = "nil"
let () = ignore main
