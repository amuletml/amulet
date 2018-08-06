let id x = x

(* Test binds with no captures *)
let (_, _) = (1, 2)

(* Test binds with one capture *)
let (a, _) = (3, 4)

(* Test binds with multiple captures *)
let (b, c) = (5, 6)

(* Test polymorphic binds *)
let (d, e) = (id, id)

let main = { a, b, c, d, e }
