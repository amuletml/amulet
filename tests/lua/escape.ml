external val ignore : 'a -> () = "nil"

let () = ignore
           { quote = "\""
           , slash = "\\"
           , line  =  "\n"
           , tab = "\t"
           , hex = "\x11"
           }
