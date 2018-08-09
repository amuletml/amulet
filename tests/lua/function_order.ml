let main (f : int -> int) g =
  let a = f 1
  let b = f 2
  let c = f 3
  g b c a

external val bottom : 'a = "nil"
let () = main bottom bottom
