(* Part of this definition includes a type like: '(∀ ( b : * ). 'b -> 'a
   -> (∀ ( b : * ). 'b -> 'a))'

   When we attempt to unify with a type of different names, we add a
   mapping saying"b == <their version of b>". However, we did not remove
   this mapping when visiting the nested b, causing the substitution to
   fail. *)

let foo : forall 'a. 'a -> forall 'b. 'b -> 'a = fun x y -> x

let main : int -> string -> int = foo foo foo
