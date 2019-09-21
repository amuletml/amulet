external val ignore : 'a -> () = "nil"

let () = ignore @@ fun f ->
  let a = f 1
  { b = f 2,
    a = a }
