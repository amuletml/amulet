class foo 'a begin
  val foo : 'a -> unit
end

class foo 'a => bar 'a begin
  val bar : 'a -> unit
  let bar = foo
end

class bar 'a => baz 'a begin
  val baz : 'a -> unit
  let baz = bar
end

class baz 'a => quux 'a begin
  val quux : 'a -> unit
  let quux = foo
end

instance foo () begin
  let foo _ =
    let _ = "this will be default"
    ()
end

instance bar () begin end
instance baz () begin end
instance quux () begin end
