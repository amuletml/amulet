type list 'a = Nil | Cons of 'a * list 'a

(* Tests the small branching factor heuristic *)
let common_prefix = function
  | 1, 2 -> "foo"
  | 1, 3 -> "bar"

let common_suffix = function
  | 1, 2 -> "foo"
  | 1, 3 -> "bar"

(* A rather nasty case where there's no one good solution. *)
let mixed_1 = function
  | true, 1, _ -> 1
  | false, 2, Nil -> 2
  | _, _, Cons _ -> 3

external val bottom : 'a = "nil"
let () = bottom { common_prefix, common_suffix, mixed_1 }
