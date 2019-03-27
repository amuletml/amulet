(* Ands merge with the current expression *)

let x = 1 and y = 1

let x = 1
and y = 1

let _ = let x = 1 and y = 1
        3

let _ = let x = 1
        and y = 1
        let x = 1 and y = 1
        1
and _ = 1
