external val ignore : 'a -> () = "nil"

let () = ignore (lazy 2, fun x -> force x)
