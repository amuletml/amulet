class eq 'a begin
  val eq : 'a -> 'a -> bool
end

instance eq () begin
  let eq () () = true
end

class eq 'a => show 'a begin
  val show : 'a -> string
  val show_tail : 'a -> string

  let show_tail x =
    if x `eq` x then
      ""
    else
      ""
end

instance show () begin
  let show () = "()"
end
