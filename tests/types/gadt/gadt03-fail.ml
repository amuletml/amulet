external val (^) : string -> string -> string = ""
external val to_string : 'a -> string = "tostring"

type term 'a =
  | B : bool -> term bool
  | I : int -> term int

let show = function
  | I x -> "I " ^ to_string x
  | B x -> "I " ^ to_string x
