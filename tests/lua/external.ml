external val rem : int -> int -> int = "function(x, y) return x % y end"

external val ignore : 'a -> () = "nil"
let () = ignore (5 `rem` 3)
