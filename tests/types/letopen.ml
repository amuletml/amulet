module Foo =
  class foo 'a begin
    val foo_it : 'a -> unit
  end
  instance foo int begin
    let foo_it _ = ()
  end

let () = Foo.( foo_it 123 )
