class a 'a begin
  val use : 'a -> unit
end

class a 'a => b 'a begin
  val reuse : 'a -> unit
end

let foo x = let () = use x in reuse x
