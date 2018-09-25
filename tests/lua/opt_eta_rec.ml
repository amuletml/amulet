(* This used to be simplified to let f = f, which is obviously invalid *)
let () =
  let f x = f x
  f ()
