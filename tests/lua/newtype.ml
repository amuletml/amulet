(* Regular, monomorphic type - gets newtyped *)
type foo = Foo of int
(* Regular, polymorphic type - gets newtyped *)
type bar 'a = Bar of 'a
(* GADT with existentials - no newtyping *)
type exists =
  | It : forall 'a. 'a -> exists

(* GADT without existentals - gets newtyped *)
type gadt 'a =
  | Mk : int -> gadt int

let main (x : forall 'a. 'a -> unit) =
  x Foo (* Doesn't get inlined *)
  x Bar (* Bar's worker has a type argument, so it'll get inlined *)
  x It (* Doesn't get inlined *)
  x Mk (* Mk's worker has a type argument, so it'll get inlined *)
