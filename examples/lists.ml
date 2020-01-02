open import "prelude.ml"

let rec range start stop =
  if start == stop then
    Nil
  else
    Cons (start, range (start + 1) stop)

let () = print ((+ 1) <$> range 1 10)
