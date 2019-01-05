
external val print : int -> unit = "print"

let id x = x

let main =
  let f = (fun _ -> g) 1
  and g = (fun _ -> f) 1
  print f
