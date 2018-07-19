let main x =
  let id x = x
  let g f = f (id x) (* non-let bound variable *)
  let h f = g f
  g (fun x -> x + 1)
  h (fun x -> x ^ "")
