class foo 'a begin
  val x : 'a -> ()
end
instance foo 'a begin let x _ = () end
instance foo () begin let x _ = () end
