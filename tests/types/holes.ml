type 'a || 'b = In1 of 'a | In2 of 'b
type 'a && 'b = And of 'a * 'b

type void = |
type not 'a = Not of 'a -> void

(* Elementary sentences of first-order logic. *)

let _ =
  let or_in1 : forall 'a 'b. 'a -> 'a || 'b = _
  let or_in2 : forall 'a 'b. 'b -> 'a || 'b = _

  let or_elim : forall 'a 'b 'r. ('a -> 'r) -> ('b -> 'r) -> 'a || 'b -> 'r = _

  let or_swap : forall 'a 'b. 'a || 'b -> 'b || 'a = _

  let and_in : forall 'a 'b. 'a -> 'b -> 'a && 'b = _
  let and_elim : forall 'a 'b 'r. 'a && 'b -> ('a -> 'b -> 'r) -> 'r = _

  let and_p1 : forall 'a 'b 'r. 'a && 'b -> 'a = _
  let and_p2 : forall 'a 'b 'r. 'a && 'b -> 'b = _

  let and_swap : forall 'a 'b. 'a && 'b -> 'b && 'a = _

  let contrapositive : forall 'p 'q. ('p -> 'q) -> (not 'q -> not 'p) = _
  let noncontradiction : forall 'p. not ('p && not 'p) = _
  let not_not_lem : forall 'p. not (not ('p || not 'p)) = _
  let ex_falso : forall 'a. void -> 'a = _

  ()

