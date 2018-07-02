let magic (x : forall 'a. 'a * ('a -> int)) =
  match x with
  | (x, f) -> f x
