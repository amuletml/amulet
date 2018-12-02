external val bottom : 'a = "print"

let () = bottom @@ fun f ->
  let a = f 1
  { b = f 2,
    a = a }
