external val ( +. )  : float -> float -> float = "function(x, y) return x + y end"
external val ( **. ) : float -> float -> float = "function(x, y) return x ^ y end"
external val ( *. )  : float -> float -> float = "function(x, y) return x * y end"
external val ( /. )  : float -> float -> float = "function(x, y) return x / y end"
external val ignore : 'a -> () = "nil"

let f @@ x = f x

let main { a, b, c } =
  (* Lower precedence *)
  ignore @@ (a +. b) *. c
  ignore @@ a *. (b +. c)

  (* Higher precedence *)
  ignore @@ (a **. b) *. c
  ignore @@ a *. (b **. c)

  (* Same precedence but different associativities *)
  ignore @@ a *. (b /. c)
  ignore @@ (a *. b) /. c

let () = ignore main
