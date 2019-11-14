external val (+) : int -> int -> int = "function(x, y) return x + y end"

let _ =
  let _ = 1 + ()
  let _ : forall 'a. 'a -> 'a = fun x -> x + 1
  ()
