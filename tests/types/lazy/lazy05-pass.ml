external val ( * ) : int -> int -> int = ""
external val ( - ) : int -> int -> int = ""
external val ( == ) : int -> int -> bool = ""

external val print : int -> unit = "print"

let fix (f : lazy 'a -> 'a) : 'a = f (fix f)

let fact : int -> int =
  fix (fun fact n -> if n == 0 then 1 else n * fact (n - 1))

let main = print (fact 10)

