@cg (import (chicken flonum))

module Public = begin
  external val ( + )  : int -> int -> int   = "+"
  external val ( * )  : int -> int -> int   = "*"
  external val ( - )  : int -> int -> int   = "-"
  external val ( ** ) : int -> int -> int   = "expt"
  external val ( / )  : int -> int -> float = "/"

  external val ( +. )  : float -> float -> float = "fp+"
  external val ( *. )  : float -> float -> float = "fp*"
  external val ( -. )  : float -> float -> float = "fp-"
  external val ( **. ) : float -> float -> float = "fpexpt"
  external val ( /. )  : float -> float -> float = "fp/"

  external val (^) : string -> string -> string = "string-append"

  external val (<.)  : float -> float -> bool = "fp<"
  external val (<=.) : float -> float -> bool = "fp<="
  external val (>.)  : float -> float -> bool = "fp>"
  external val (>=.) : float -> float -> bool = "fp>="
end

external val int_eq    : int -> int -> bool       = "="
external val float_eq  : float -> float -> bool   = "fp="
external val string_eq : string -> string -> bool = "string=?"

external val lt_int : int -> int -> bool = "<"
external val lte_int : int -> int -> bool = "<="
external val gt_int : int -> int -> bool = ">"
external val gte_int : int -> int -> bool = ">="

external val lt_string : string -> string -> bool = "string<?"
external val lte_string : string -> string -> bool = "string<=?"
external val gt_string : string -> string -> bool = "string>?"
external val gte_string : string -> string -> bool = "string>=?"

external val string_of_int : int -> string = "number->string"
external val string_of_float : float -> string = "number->string"

