external val print : string -> unit = "print"

type option 'a = None | Some of 'a

let r = ref None

let () =
  r := Some 1
  print (!r)
