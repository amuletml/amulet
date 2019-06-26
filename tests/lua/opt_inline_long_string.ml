external val print : string -> () = "print"

type maybe 'a = None | Just of 'a

let fromMaybe d = function
  | Just n -> n
  | None -> d

let () =
  let "123456789" as x = fromMaybe "no string" (Just "123456789")
  print x
