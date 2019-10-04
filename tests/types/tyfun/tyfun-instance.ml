type function foo 'a begin end

class bar 'a begin
  val foo : 'a () -> 'a ()
end

instance bar foo begin end
