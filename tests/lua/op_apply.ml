let id x = x

external val bottom : 'a = "nil"
let () = bottom { op = (@@)
                , app = id @@ 2
                , rsec = (@@2)
                , lsec = (id@@)
                }
