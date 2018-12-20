external val bottom : 'a -> () = "nil"

let () = bottom [1,2,3]

let () =
  let [a,2,3] = [1,2,3]
  bottom a

let () = bottom [ let a = 1 in 2 ]

let () = bottom [ (a, b) | with a <- [1, 2, 3], with b <- [ 4, 5, 6 ] ]
let () = bottom [ (a, b) | with a <- [ 1, 2, 3 ], let b = a + 1, with c <- [ b, a ] ]
