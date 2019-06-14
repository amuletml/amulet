external val (>>=) : forall 'a 'b. list 'a -> ('a -> list 'b) -> list 'b = "bind"
external val pure : forall 'a. 'a -> list 'a = "pure"

let _ = begin
  with x <- [ 1, 2, 3 ]
  with y <- [ 4, 5, 6]
  pure (x, y)
end
