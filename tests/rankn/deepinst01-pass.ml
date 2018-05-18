(* This used to be ruled out by the predicativity checker, but it works out OK
 * with deep instantiation *)

let foo : forall 'a. 'a -> forall 'b. 'b -> 'a = fun x y -> x

let main = foo foo foo () ()
