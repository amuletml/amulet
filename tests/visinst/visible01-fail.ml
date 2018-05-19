let foo : forall ('a : type) -> 'a -> 'a = fun x -> x

let bar = foo @{string} 1
and baz = foo @{unit} 1
