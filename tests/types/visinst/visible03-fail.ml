let foo : forall ('a : type) -> 'a -> int = fun _ -> 0

let main = foo 1
