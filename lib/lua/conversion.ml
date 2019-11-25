open import "./../amulet/option.ml"

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
