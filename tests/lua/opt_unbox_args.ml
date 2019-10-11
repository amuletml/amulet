external val ( * ) : int -> int -> int = "function(x, y) return x * y end"
external val ( - ) : int -> int -> int = "function(x, y) return x * y end"
external val ( <= ) : int -> int -> bool = "function(x, y) return x > y end"

let fib' x acc = if x <= 1 then acc
                 else fib' (x - 1) (acc * x)
let fib x = fib' x 1

external val ignore : 'a -> () = "nil"
let () = ignore (fib 10)
