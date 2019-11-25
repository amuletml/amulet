open import "./../amulet/base.ml"

external val put_line : string -> unit = "print"
external val put_bytes : string -> unit = "display"

let print x = put_line (show x)
