type list 'a = Nil | Cons of 'a * list 'a

let main x = let y = (1, x)
             in match x with
                | Nil -> fun _ -> y
                | Cons _ -> fun x -> (x, Nil)

external val bottom : 'a = "nil"
let () = bottom main
