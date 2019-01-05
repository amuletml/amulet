external val print : string -> unit = "print"

let _ : lazy int =
  lazy (let _ = print "foo" in 123)
