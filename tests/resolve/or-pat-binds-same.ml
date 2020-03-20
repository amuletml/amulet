type t = A of int | B
let bar (A _ | B) = 0
