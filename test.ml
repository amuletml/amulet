val foreign read "io.read" : string -> string;
val foreign print "print" : string -> unit;
val foreign number_of_string "tonumber" : string -> int;
val foreign string_of_number "tonumber" : int -> string;

let fact n
  = if (n == 0) then
    1
  else
    n * fact (n - 1)
and main _ =
  let line = read "*l"
  and num = number_of_string line
  and fac = fact num
   in print (line ^ " factorial is " ^ (string_of_number fac))
