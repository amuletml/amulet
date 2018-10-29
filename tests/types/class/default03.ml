class foo 'a begin
  val x : 'a -> int
  val y : 'a -> int
  let y _ = 0
end

instance foo int begin
  let x _ = 0
end
