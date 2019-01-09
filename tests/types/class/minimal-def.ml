class foo 'a begin
  val f : 'a -> unit
  val g : 'a -> unit
  let f x = g x
  let g x = f x

  val h : 'a -> unit
  let h x = f x

  val i : 'a -> unit
  val j : 'a -> unit

  val k : 'a -> unit
  let k x = m x
  val m : 'a -> unit
  let m x = l x
  val l : 'a -> unit
  let l x = k x
end

instance foo int begin end

