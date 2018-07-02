external val to_string : 'a -> string = "tostring"

type term 'a =
  | B : bool -> term bool
  | I : int -> term int

let show : forall 'a. term 'a -> string = function
  | I x -> "I " ^ to_string x
  | B x -> "I " ^ to_string x
