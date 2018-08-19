type exists =
  | It : forall 'a. 'a * ('a -> int) -> exists

let f x =
  let It (foo, f) = x
  f foo
