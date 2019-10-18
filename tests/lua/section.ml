external val ( * ) : int -> int -> int = "function(x, y) return x * y end"
external val ( + ) : int -> int -> int = "function(x, y) return x + y end"
external val ( / ) : int -> int -> float = "function(x, y) return x / y end"
(* Ensure that operator and record sections result in semi-sane code. *)
let main = ( (+), (2*), (/2), ((.foo) : { foo : int } -> int) )

external val ignore : 'a -> () = "nil"
let () = ignore main
