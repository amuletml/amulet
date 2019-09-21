(* This ensures that expressions which are consumed in a different order to the
   way they are defined results in some level of a sensible tree.

  It's worth noting that the result is not (currently) perfect, as `c` could be
   declared inline, but it's a useful test.  *)

let main f =
  let a = f 1
  let b = f 2
  let c = f 3
  ( b, c, a )

external val ignore : 'a -> () = "nil"
let () = ignore main
