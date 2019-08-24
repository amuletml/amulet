type file

external val file_close : file -> () = "io.close"
external val file_read : string -> string = "io.read"
external val print : string -> unit = "print"

let file_load fhandle =
  let output = file_read "*all"
  file_close fhandle
  output
