let auto' = fun (x : forall 'a. 'a -> 'a) -> x
let choose (x : 'a) (_ : 'a) = x

let id x = x

let t = choose id auto'
