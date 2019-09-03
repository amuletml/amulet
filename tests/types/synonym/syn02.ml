type two_of 'a <- 'a * 'a
type two_int <- two_of int

let foo : two_int = (1, 1)
let bar : two_of int = foo
let bar : int * int = foo

