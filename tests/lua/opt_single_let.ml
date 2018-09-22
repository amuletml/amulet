let f x =
  let () = x ()
  ()

type z = Z

let g x =
  let Z = x ()
  Z

external val bottom : 'a = "nil"
let () = bottom { f, g }
