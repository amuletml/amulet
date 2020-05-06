external val ignore : 'a -> () = "nil"

let f @@ x = f x

(* Flatten nested record updates *)
let () = ignore @@ fun x ->
  let x = { x with a = 1, c = 2 }
  let x = { x with a = 2 }
  let x = { x with b = 5 }
  let x = { x with a = 3 }
  x

(* We should reuse x here, rather than creating two copies. *)
let () = ignore @@ fun x ->
  let x = { x with a = 1 }
  (x, { x with a = 1 })

(* But not here. *)
let () = ignore @@ fun x ->
  let x = { x with a = 1 }
  (x, { x with a = 2 })
