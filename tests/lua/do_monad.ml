external val (>>=) : forall 'a 'b. list 'a -> ('a -> list 'b) -> list 'b = "bind"
external val pure : forall 'a. 'a -> list 'a = "pure"

let _ =
  let! x = [ 1, 2, 3 ]
  let! y = [ 4, 5, 6 ]
  pure (x, y)
