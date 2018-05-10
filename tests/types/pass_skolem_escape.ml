let escapes (f : forall 'a. 'a -> 'b) = f "foo"
let escape_pass = escapes (fun x -> 0)
