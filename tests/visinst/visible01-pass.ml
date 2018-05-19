let foo : forall ('a : type) -> 'a -> 'a = fun x -> x

let bar = foo @{int} 1
and baz = foo ? 1
