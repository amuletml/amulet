external val (+) : int -> int -> int = ""

let apply
  : forall 'row 'a 'b.
    { 'row | apply : forall 'a 'b. ('a -> 'b) -> 'a -> 'b }
    -> ('a -> 'b) -> 'a -> 'b
  = fun r -> r.apply

let main =
  apply ({ apply = fun x -> x }) (fun x -> x + 1) 123
