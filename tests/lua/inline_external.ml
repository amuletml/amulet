module Ref =
  type ref 'a
  external val new : 'a -> ref 'a = "function(x) return { x } end"
  external val get : ref 'a -> 'a = "function(x) return x[1] end"
  external val set : ref 'a -> 'a -> () = "function(x, y) x[1] = y end"

external val print : 'a -> () = "print"

let () =
  let r = Ref.new 0
  print (Ref.get r)
  r `Ref.set` 1
  print (Ref.get r)
