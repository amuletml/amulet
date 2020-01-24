module Public = struct
  external val ( + )  : int -> int -> int   = "function (x, y) return x + y end"
  external val ( * )  : int -> int -> int   = "function (x, y) return x * y end"
  external val ( - )  : int -> int -> int   = "function (x, y) return x - y end"
  external val ( ** ) : int -> int -> int   = "function (x, y) return x ^ y end"
  external val ( / )  : int -> int -> float = "function (x, y) return x / y end"

  external val ( +. )  : float -> float -> float = "function (x, y) return x + y end"
  external val ( *. )  : float -> float -> float = "function (x, y) return x * y end"
  external val ( -. )  : float -> float -> float = "function (x, y) return x - y end"
  external val ( **. ) : float -> float -> float = "function (x, y) return x ^ y end"
  external val ( /. )  : float -> float -> float = "function (x, y) return x / y end"

  external val (^) : string -> string -> string = "function(x, y) return x .. y end"

  external val (<.)  : float -> float -> bool = "function (x, y) return x < y end"
  external val (<=.) : float -> float -> bool = "function (x, y) return x <= y end"
  external val (>.)  : float -> float -> bool = "function (x, y) return x > y end"
  external val (>=.) : float -> float -> bool = "function (x, y) return x >= y end"
end

external val int_eq    : int -> int -> bool       = "function(x, y) return x == y end"
external val float_eq  : float -> float -> bool   = "function(x, y) return x == y end"
external val string_eq : string -> string -> bool = "function(x, y) return x == y end"

external val lt_int : int -> int -> bool = "function (x, y) return x < y end"
external val lte_int : int -> int -> bool = "function (x, y) return x <= y end"
external val gt_int : int -> int -> bool = "function (x, y) return x > y end"
external val gte_int : int -> int -> bool = "function (x, y) return x >= y end"

external val lt_string : string -> string -> bool = "function (x, y) return x < y end"
external val lte_string : string -> string -> bool = "function (x, y) return x <= y end"
external val gt_string : string -> string -> bool = "function (x, y) return x > y end"
external val gte_string : string -> string -> bool = "function (x, y) return x >= y end"

external val string_of_int : int -> string = "tostring"
external val string_of_float : float -> string = "tostring"
