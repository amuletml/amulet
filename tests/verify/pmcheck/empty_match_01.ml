(* non-exhaustive *)
let f1 : forall 'a. int -> 'a =
  fun x -> match x with ()

type foo = A | B

(* non-exhaustive *)
let f2 (x : foo) =
  match x with ()

(* exhaustive *)
type void = |
let f3 (x : void) =
  match x with ()
