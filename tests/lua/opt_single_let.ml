let f x =
  let () = x ()
  ()

type z = Z

let g x =
  let Z = x ()
  Z

external val ignore : 'a -> () = "nil"
let () = ignore { f, g }
