type function f 'a : type begin
  f int = string
  f string = int
end

type ev 'a =
  | Int : int -> ev int
  | Str : string -> ev string

let foo (x : ev 'a) : f 'a =
  match x with
  | Int _ -> ""
  | Str _ -> 0
