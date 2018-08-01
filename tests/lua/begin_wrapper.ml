external val print : 'a -> unit = "print"

let andalso a b =
  match a with
  | true -> force b
  | false -> false

let main =
  print (andalso false (begin
    print "foo"
    true
  end))
