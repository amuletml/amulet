let f1 p k = function
| 0 when p () -> k 0
| _ -> k 3 (* Should be shared. *)

let f2 p = function
| 0 when p () -> 0
| _ -> 3 (* Should not be shared, as it is an atom. *)

let f3 p k = function
| 0, _ -> k 0
| 1, 2 -> k 1
| _, 1  when p () -> k 2
| _, _ -> k 3
(* Guard and pattern should be shared. *)
