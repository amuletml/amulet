let if_then_else c t e =
  if c then force t else force e

external val print : string -> unit = "print"

let main = if_then_else true (print "hello there") (Lazy (fun x -> x))

(* one branch explicit, one implicit *)
