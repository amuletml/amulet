external val (+) : int -> int -> int = ""

class functor 'f begin
  val map : forall 'a 'b. ('a -> 'b) -> 'f 'a -> 'f 'b
end

instance functor ((->) 'a) begin
  let map f g x = f (g x)
end

instance functor (( * ) 'a) begin
  let map f (x, y) = (x, f y)
end

let (1, 1, 3) = map map map (+1) (1, 1, 2)
