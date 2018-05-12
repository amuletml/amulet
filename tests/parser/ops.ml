let x |> f = f x
let f <| x = f x

let a `add` b = a + b

let main = (2 |> (3+)) `add` 3
