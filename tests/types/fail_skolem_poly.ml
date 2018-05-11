let poly (f : forall 'a. 'a -> 'a) = f ()
let poly_fail = poly (fun x -> x + 1)
