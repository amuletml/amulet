let x |> f = f x
let f :@ x = f x
let poly (f : forall 'a. 'a -> 'a) = (f 123, f true)
let id x = x

let a = id |> poly
let b = poly :@ id
