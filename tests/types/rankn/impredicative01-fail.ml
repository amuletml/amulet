(* This used to be a very simple test regarding polytypes under constructors,
 * but we allow that *)

type id 'a = Id of 'a

let foo : id (forall 'a. 'a -> 'a) = Id (fun x -> x)
