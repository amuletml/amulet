external val use : 'a -> unit = "print"

class show 'a begin
  val show : 'a -> string
  val show_tail : 'a -> string

  let show_tail x = "tail" ^ show x
end

instance show () begin
  let show () = "()"
end

let _ = use @@ (show_tail : unit -> string)
