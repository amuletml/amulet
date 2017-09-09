val foreign read "io.read" : string -> string;
let main = fun a ->
  let k = read "*l"
   in begin
     print k;
     print k
   end

