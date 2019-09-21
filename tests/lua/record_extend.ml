let main x = { x with x = 1 }

external val ignore : 'a -> () = "nil"
let () = ignore main
