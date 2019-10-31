(* This used to be simplified to let f = f, which is obviously invalid *)
let () =
  let rec f x = f x
  f ()
