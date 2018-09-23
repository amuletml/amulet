external val print : string -> unit = "print"

let _ : lazy int =
  let _ = print "foo"
  123
