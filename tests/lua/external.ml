external val rem : int -> int -> int = "function(x, y) return x % y end"

external val bottom : 'a = "nil"
let () = bottom (5 `rem` 3)
