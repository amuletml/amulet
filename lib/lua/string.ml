external val substring    : string -> int -> int -> string = "string.sub"
external val length       : string -> int = "string.len"

external val char_code_at : string -> int -> int = "string.byte"
external val char_of_code : int -> string = "string.char"

external val to_lower : string -> string = "string.lower"
external val to_upper : string -> string = "string.upper"
external val repeat   : string -> int -> string = "string.rep"
external val reverse  : string -> string = "string.reverse"
