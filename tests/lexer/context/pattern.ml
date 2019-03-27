let a = function
        | _ -> 1

let b x =
  let inner x = x * 2
  match x with
  | { x = x, y = y } -> x + y
  inner 2

let c = ()
