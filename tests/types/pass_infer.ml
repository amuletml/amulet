(* test basic HM inference *)

let a = true
and b = false

let x = 1
let f x = x + 1
let f' = (+) 1

let f & g = fun x -> f (g x)

