let main x = let y = (1, x)
             in match x with
                | Nil -> fun _ -> y
                | Cons _ -> fun x -> (x, Nil)

external val ignore : 'a -> () = "nil"
let () = ignore main
