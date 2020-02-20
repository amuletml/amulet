private module Intrinsics = import "./intrinsics.ml"

include Intrinsics

module Public = struct
  include Public

  external val (>.)  : float -> float -> bool = "function (x, y) return x > y end"
  external val (>=.) : float -> float -> bool = "function (x, y) return x >= y end"
end
external val gt_int  : int -> int -> bool = "function (x, y) return x > y end"
external val gte_int : int -> int -> bool = "function (x, y) return x >= y end"

external val gt_string  : string -> string -> bool = "function (x, y) return x > y end"
external val gte_string : string -> string -> bool = "function (x, y) return x >= y end"

external val string_of_int : int -> string = "tostring"
external val string_of_float : float -> string = "tostring"
