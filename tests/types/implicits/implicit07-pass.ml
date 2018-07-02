external val print : 'a -> unit = "print"

let given (x : 'a) (f : 'a => 'b) : 'b =
  let implicit x' = x
  f

let foo : int => int = fun x -> x + 2

let main =
  let implicit x = 3
  print (foo : int)
  given 2 foo

