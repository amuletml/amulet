external val bottom : 'a -> () = "nil"

let main { a, b, c } =
  (* Lower precedence *)
  bottom @@ (a +. b) *. c
  bottom @@ a *. (b +. c)

  (* Higher precedence *)
  bottom @@ (a **. b) *. c
  bottom @@ a *. (b **. c)

  (* Same precedence but different associativities *)
  bottom @@ a *. (b /. c)
  bottom @@ (a *. b) /. c

let () = bottom main
