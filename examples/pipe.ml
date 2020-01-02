open import "amulet/base.ml"

let ( <| ) f x = f x
let ( |> ) x f = f x

let f x =
  x + 1
  |> ( + 2)
  |> ( / 2)
