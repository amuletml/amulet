type t 'a =
  | T1 : t int
  | T2 : t bool

(* exhaustive *)
let f1 : forall 'a. t 'a -> t 'a -> bool =
  function
    | T1 -> fun T1 -> true
    | T2 -> fun T2 -> false

(* exhaustive *)
let f2 : forall 'a. t 'a * t 'a -> bool =
  function
    | T1, T1 -> true
    | T2, T2 -> false
