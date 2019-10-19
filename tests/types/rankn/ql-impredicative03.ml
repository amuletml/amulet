let map f xs = [ f x | with x <- xs ]
let poly (f : forall 'a. 'a -> 'a) = (f 123, f true)

let single x = [x]

let x = map poly (single (fun x -> x))
