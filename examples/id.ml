external val (+) : int -> int -> int = ""

external val print : int -> unit = "print"
let id x = x

let () = print (((id id) 2) + 2)
