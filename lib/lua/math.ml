external val acos  : float -> float = "math.acos"
external val asin  : float -> float = "math.asin"
external val atan  : float -> float = "math.atan"
external val cos   : float -> float = "math.cos"
external val cosh  : float -> float = "math.cosh"
external val deg   : float -> float = "math.deg"
external val exp   : float -> float = "math.exp"
external val floor : float -> float = "math.floor"
external val log   : float -> float = "math.log"
external val log10 : float -> float = "math.log10"
external val rad   : float -> float = "math.rad"
external val sin   : float -> float = "math.sin"
external val sinh  : float -> float = "math.sinh"
external val tan   : float -> float = "math.tan"
external val tanh  : float -> float = "math.tanh"
external val sqrt  : float -> float = "math.sqrt"

external val floor : float -> int = "math.floor"
external val ceil  : float -> int = "math.ceil"

external val modf  : float -> int * float =
  "function(x) \
    local int, float = math.modf(x) \
    return { _1 = int, _2 = float } \
   end"

external val pi : float = "math.pi"

external val positive_infty : float = "math.huge"
external val negative_infty : float = "-math.huge"
external val nan            : float = "-(0/0)"

external val is_nan : float -> bool =
  "function(x) return (type(x) == 'number') and x ~= x end"

external val seed_random  : float -> unit = "math.randomseed"
external val random_float : unit -> float =
  "function(x) return math.random() end"
external val random_int : int -> int -> int = "math.random"
