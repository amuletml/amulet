(* non-exhaustive *)
let f1 : forall 'a. int -> 'a = function ()

type foo = A | B

(* non-exhaustive *)
let f2 : foo -> () = function ()

(* exhaustive *)
type void = |
let f3 : void -> () = function ()
