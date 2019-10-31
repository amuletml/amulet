class show 'a begin
  val show : 'a -> string
end

let rec foo x = let _ = show x in bar x
and bar x = foo x
