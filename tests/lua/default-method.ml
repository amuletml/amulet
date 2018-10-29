class show 'a begin
  val show : 'a -> string
  val show_tail : 'a -> string

  let show_tail x = "tail" ^ show x
end

instance show () begin
  let show () = "()"
end

let "" = show_tail ()
