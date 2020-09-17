module Public = struct
  external val ( + )  : int -> int -> int   = "%int.add"
  external val ( * )  : int -> int -> int   = "%int.mul"
  external val ( - )  : int -> int -> int   = "%int.sub"
  external val ( ** ) : int -> int -> float = "%int.pow"
  external val ( / )  : int -> int -> float = "%int.div"

  external val ( +. )  : float -> float -> float = "%float.add"
  external val ( *. )  : float -> float -> float = "%float.mul"
  external val ( -. )  : float -> float -> float = "%float.sub"
  external val ( **. ) : float -> float -> float = "%float.pow"
  external val ( /. )  : float -> float -> float = "%float.div"

  external val (^) : string -> string -> string = "%string.concat"

  external val (<.)  : float -> float -> bool = "%float.lt"
  external val (<=.) : float -> float -> bool = "%float.le"
end

external val int_eq    : int -> int -> bool       = "%int.eq"
external val float_eq  : float -> float -> bool   = "%float.eq"
external val string_eq : string -> string -> bool = "%string.eq"
external val bool_eq   : bool -> bool -> bool     = "%bool.eq"

external val lt_int  : int -> int -> bool = "%int.lt"
external val lte_int : int -> int -> bool = "%int.le"

external val lt_string  : string -> string -> bool = "%string.lt"
external val lte_string : string -> string -> bool = "%string.le"
