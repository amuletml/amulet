external val ( + ) : int -> int -> int = "function(x, y) return x + y end"
let rec f x = g x + 1
and g x = f x + 1

external val ignore : 'a -> () = "nil"
let () = ignore f
