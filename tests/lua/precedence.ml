external val ignore : 'a -> () = "nil"

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
