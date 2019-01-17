external val use : 'a -> unit = "print"

class show 'a begin
  val show : 'a -> string

  val show' : 'a -> string
  let show' = show
end

instance show () begin
  let show () = "()"
end

let () = use (show : unit -> string)
