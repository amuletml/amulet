let partial_1 =
  match { x = 1, y = 1 } with
  | { x } -> ()

let partial_2 =
  match { x = 1 } with
  | { } -> ()
