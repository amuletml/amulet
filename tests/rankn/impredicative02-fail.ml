(* A different version of lint/shadow_forall.ml, which is caught by the
 * predicativity checker *)

let foo : forall 'a. 'a -> forall 'b. 'b -> 'a = fun x y -> x

let main = (foo foo foo) () ()
