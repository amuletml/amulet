external val ignore : 'a -> () = "nil"
external val print : 'a -> () = "print"
external val bool : bool = "true"

let a && b = if a then b else false
let a || b = if a then true else b
let not a = if a then false else true

let () = ignore { ands = (&&), ors = ( || ), not }

let () = ignore @@ fun () ->
  if bool
  then print "L"
  else print "R"
       print "R"

let () = ignore @@ fun () ->
  if bool
  then print "L"
       print "L"
  else print "R"

let () = if bool then print "Hello" else ()

let () = if bool then () else print "Hello"

let () = ignore @@ function
  | 0 -> ()
  | _ -> print "Not 0"
         print "Extra text"
