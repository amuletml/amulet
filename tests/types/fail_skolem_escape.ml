let escapes (f : forall 'a. 'a -> 'b) = f "foo"
let escape_fail = escapes (fun x -> x)
