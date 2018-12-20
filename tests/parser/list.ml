let _ = [1,2,3]
let [1,2,3] = [1,2,3]

let _ = [ (a, b) | with a <- [1, 2, 3], with b <- [ 4, 5, 6 ] ]
let _ = [ (a, b) | with a <- [ 1, 2, 3 ], let b = a + 1, with c <- [ b, a ] ]
