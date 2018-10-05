external val bottom : 'a -> () = "nil"

let () = bottom (lazy 2, fun x -> force x)
