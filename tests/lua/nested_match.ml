type list 'a = Nil | Cons of 'a * list 'a

let main = function
 | Nil, _ -> 1
 | _, Nil -> 2
 | Cons (_, _), Cons (_, _) -> 3

external val bottom : 'a = "nil"
let () = bottom main
