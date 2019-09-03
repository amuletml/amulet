type two_of 'a <- 'a * 'a

let foo : list (two_of int) = [(1, 1)]
let bar (x : list (int * int)) = ()

let _ = bar foo
