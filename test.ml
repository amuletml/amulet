val foreign read "io.read" : string -> string;
val foreign print "print" : string -> unit;
let readLine = read "*l"
and main a =
  let k = read "*l"
   in begin
     if true then
       print k
     else
       print "not k"
   end

