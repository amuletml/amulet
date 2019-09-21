let main = function
  | { a = { a } as b, c } -> a + b.b + c

external val ignore : 'a -> () = "nil"
let () = ignore main
