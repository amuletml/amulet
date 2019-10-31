external val ( + ) : int -> int -> int = "function(x, y) return x + y end"
let rec main { x } = x + main { x }
let _ = main { x = 1 }
