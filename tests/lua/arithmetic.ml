let main f =
  let a = f 1
  let b = f 2
  let c = (+) a
  c b

external val bottom : 'a = "nil"
let () = bottom (main bottom)
