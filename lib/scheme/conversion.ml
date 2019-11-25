open import "./../amulet/option.ml"

@cg (define (prim-parse-int Some None x)
@cg   (if (and (string->number x)
@cg       (exact? (string->number x)))
@cg     (Some (string->number x))
@cg     None))
external private val prim_parse_int : (int -> option int) -> option int -> string -> option int =
  "prim-parse-int"

let parse_int = prim_parse_int Some None

@cg (define (prim-parse-float Some None x)
@cg   (if (string->number x)
@cg     (Some (string->number x))
@cg     None))
external private val prim_parse_float : (float -> option float) -> option float -> string -> option float =
  "prim-parse-float"

let parse_float = prim_parse_float Some None

external val float_of_int : int -> float = "exact->inexact"
external val int_of_float : float -> int = "(o inexact->exact round)"

external val floor : float -> int = "(o inexact->exact floor)"
