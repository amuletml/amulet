let id x = x

let foo : forall 'a. 'a -> 'a = id (fun x -> x)
