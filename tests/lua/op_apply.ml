let id x = x

let f @@ x = f x

external val ignore : 'a -> () = "nil"
let () = ignore { op = (@@)
                , app = id @@ 2
                , rsec = (@@2)
                , lsec = (id@@)
                }
