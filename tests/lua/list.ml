external val (+) : int -> int -> int =
  "function(x, y) return x + y end"

external val ignore : 'a -> () = "nil"

let () = ignore [1,2,3]

let () =
  let [a,2,3] = [1,2,3]
  ignore a

let () = ignore [ let a = 1 in 2 ]

let () = ignore [ (a, b) | with a <- [1, 2, 3], with b <- [ 4, 5, 6 ] ]
let () = ignore [ (a, b) | with a <- [ 1, 2, 3 ], let b = a + 1, with c <- [ b, a ] ]
