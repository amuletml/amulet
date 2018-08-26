(* Unit bind *)
let () = ()

(* Tuple bind *)
let (_, _) = (1, 2)

(* Tuple bind with annotation *)
let (_, _) : int * int = (1, 2)

(* Record bind *)
let { a = _, b } = (3, 4)

(* Constructor bind *)
let Some x = Some 1
