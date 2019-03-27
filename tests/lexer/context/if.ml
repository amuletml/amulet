(* If expressions allow for multiple blocks *)
let _ =
  if true then
    1
    1
  else
    1
    1

(* Allow if/else on a new line *)
let _ =
  if true
  then
    1
  else
    1

(* Allow "early return" ifs *)
let _ =
  if true then 1 else
  if true then 1 else
    1
    1

(* Allow "else if" style expressions *)
let _ =
  if true then
    1
  else if true then
    1
  else
    1

(* Nesting *)
let _ =
  if true then
    if true then
      1
    else
      1
  else
    1

(*
(* TODO: Weird alignment of ifs *)
let _ = if true
  1
else
  2
*)
