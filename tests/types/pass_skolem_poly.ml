let poly (f : forall 'a. 'a -> 'a) = f ()
let poly_pass = poly (fun x -> x)
