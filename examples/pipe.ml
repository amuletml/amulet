external val ( + ) : int -> int -> int = ""
external val ( / ) : int -> int -> float = ""

let ( <| ) f x = f x
let ( |> ) x f = f x

let (>>) g f x = f (g x)
let (<<) f g x = f (g x)

let f x =
  x + 1
  |> (+2)
  |> (/2)
