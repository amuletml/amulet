(* We can simplify terms of the form `let x = ... in y` where `x` and `y`
   have the same type and that type only has one possible value. *)
module NoArgs =
  type mono = Mono

  external val go : () -> mono = "nil"

  let main =
    let other = Mono
    let _ = go ()
    other

(* A single constructor type with an argument should not be simplified *)
module WithArgs =
  type mono = Mono of int

  external val go : () -> mono = "nil"

  let main =
    let other = Mono 2
    let _ = go ()
    other

external val bottom : 'a = "nil"
let () = bottom (NoArgs.main, WithArgs.main)
