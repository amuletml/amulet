type exists =
  | It : forall 'a. 'a * ('a -> int) -> exists

let It (foo, finger) = It (1, fun x -> x)
