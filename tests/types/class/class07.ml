class read 'a begin
  val read : string -> 'a
end

class show 'a begin
  val show : 'a -> string
end

let foo x = show (read (show x))
