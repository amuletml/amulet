type id 'a = Id of 'a

let pure (f : forall 'a. 'a -> 'f 'a) = f 1
let pure_pass = pure Id
