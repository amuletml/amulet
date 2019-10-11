external val (+) : int -> int -> int = ""

(* Slight variation on record05, where the forall is later in the type *)
let apply
  : forall 'row.
    { 'row | apply : forall 'a 'b. ('a -> 'b) -> 'a -> 'b }
    -> forall 'a 'b. ('a -> 'b) -> 'a -> 'b
  = fun r -> r.apply

let main =
  apply ({ apply = fun x -> x }) (fun x -> x + 1) 123
