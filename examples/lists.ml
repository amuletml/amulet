open import "prelude.ml"

let rec range_from_to start stop =
  if start == stop then
    Nil
  else
    start :: [start + 1 .. stop]

let () = print ((+ 1) <$> [1 .. 10])
