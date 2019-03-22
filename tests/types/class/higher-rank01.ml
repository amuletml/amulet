external val unsafe_coerce : 'a -> 'b = "function(x) return x end"
type foo 'a = Foo of int

class fooable 'a begin
  val foo : foo 'a
end

type gift 'a 'b = Give of fooable 'a => 'b

let with_foo (x : foo 'a) (k : fooable 'a => 'b) =
  let gift = Give @'a @'b k
  unsafe_coerce gift x
