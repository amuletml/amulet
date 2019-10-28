include import "./amulet/base.ml"
include import "./amulet/option.ml"
include import "./amulet/list.ml"
module Typeable = import "./amulet/typeable.ml"
module Exception = import "./amulet/exception.ml"

include import "./data/foldable.ml"
include import "./data/traversable.ml"

include import "./data/enumeration.ml"

module Io = import "./lua/io.ml"

external private val prim_parse_int : (int -> option int) -> option int -> string -> option int =
  "function(Some, None, x) \
     if tonumber(x) and ((math.modf(tonumber(x))) == tonumber(x)) then \
       return Some(tonumber(x)) \
     else \
       return None \
     end \
   end"

let parse_int = prim_parse_int Some None

external private val prim_parse_float : (float -> option float) -> option float -> string -> option float =
  "function(Some, None, x) \
     if tonumber(x) then \
       return Some(tonumber(x)) \
     else \
       return None \
     end \
   end"

let parse_float = prim_parse_float Some None

external val float_of_int : int -> float = "function(x) return x end"
external val int_of_float : float -> int =
  "function(x) \
    if x >= 0 then \
      return math.floor(x + 0.5) \
    else \
      return math.ceil(x - 0.5) \
    end \
   end"

external val floor : float -> int = "math.floor"

let x // y =
  let open Exception
  if y == 0 then
    throw DivideBy0
  else
    floor (x / y)

let put_line = Io.put_line
let put_bytes = Io.put_bytes
let print = Io.print
