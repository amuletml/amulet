val foreign read "io.read" : string -> string;
val foreign print "print" : string -> unit;
val foreign number_of_string "tonumber" : string -> int;

let main _ =
  let line = read "*l"
  and num = number_of_string line
   in if (num <> 10) then
     print "number was not 10!"
   else
     print "number was 10!"
