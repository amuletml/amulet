external val print : int -> unit = "print" ;;
let id x = x ;;

let main _ = print (((id id) 2) + 2)
