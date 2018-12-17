external val bottom : 'a -> () = "nil"

let () = bottom
           { quote = "\""
           , slash = "\\"
           , line  =  "\n"
           , tab = "\t"
           , hex = "\x11"
           }
