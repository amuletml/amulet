type a = A1 of () | A2 of int

let func f = function
| A2 _ when true -> ()
| x -> f x

let func f = function
| A2 _, _ when true -> ()
| x, y -> f x y
