external val print : string -> unit = "print"

type option 'a = None | Some of 'a


let () =
  let r = ref None
  r := Some 1
  print (!r)
