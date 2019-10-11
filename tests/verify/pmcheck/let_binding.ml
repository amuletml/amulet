external val (+) : int -> int -> int = ""

let f g =
  let () = g ()
  ()

let f g =
  let 0 = g ()
  ()

let f g =
  let { x, y } = g ()
  x + y
