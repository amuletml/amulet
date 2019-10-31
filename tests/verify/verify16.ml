external val print : int -> unit = "print"

let id x = x

let main =
  let rec f = id (fun _ -> g) 1
  and g = id (fun _ -> f) 1
  print f
