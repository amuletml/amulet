let pure (f : forall 'a. 'a -> 'f 'a) = f 1
let pure_fail = pure (fun x -> x)
