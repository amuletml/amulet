(* Op functions *)
let x |> f = f x
let f <| x = f x

(* Op binds *)
let (£) a = (1*)
let (€) = (1.12*)

let f = function
  | ( (+), (-) ) -> 1 + 2 - 3

(* Backtick binds *)
let a `add` b = a + b

(* Backtick usage *)
let main = (2 |> (3+)) `add` 3
