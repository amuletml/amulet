type a = A of a
type b = B1 | B2

type c 'a =
  | C1 : c int
  | C2 : c bool

type d 'a 'b =
  | D2 : d int bool
  | D2 : d bool string

type t1 'a = T1 of 'a
type t2 'a = T2 of 'a

(* exhaustive *)
let f1 (x : t1 a) : forall 'a. 'a =
  match x with ()

(* non-exhaustive *)
let f2 (x : t1 b) : forall 'a. 'a =
  match x with ()

(* exhaustive *)
let f3 (x : c string) : forall 'a. 'a =
  match x with ()
