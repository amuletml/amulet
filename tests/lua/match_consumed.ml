(* This ensures that matches which are not in the tail position correctly
   pop all pending expressions on the stack and generate the appropriate
   declaration before the join.
   *)

external val (+) : int -> int -> int =
  "function(x, y) return x + y end"
external val ( * ) : int -> int -> int =
  "function(x, y) return x * y end"

type option 't = None | Some of 't

let main f x =
  let a = f 1
  let b = match x with
          | None -> 0
          | Some x -> x * 2
  in f (a + b)

external val ignore : 'a -> () = "nil"
let () = ignore main
