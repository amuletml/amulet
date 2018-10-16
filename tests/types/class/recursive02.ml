class show 'a begin
  val show : 'a -> string
end

class show 'a => dostuff 'a begin
  val stuff : 'a -> unit
end

let foo x = let _ = show x in bar x
and bar x = stuff x; foo x
