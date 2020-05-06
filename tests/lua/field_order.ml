external val ignore : 'a -> () = "nil"

let f @@ x = f x

let () = ignore @@ fun f ->
  let a = f 1
  { b = f 2,
    a = a }
