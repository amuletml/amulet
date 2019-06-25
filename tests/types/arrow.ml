class functor 'f begin
  val map : forall 'a 'b. ('a -> 'b) -> 'f 'a -> 'f 'b
end

instance functor ((->) 'r) begin
  let map f g = fun x -> f (g x)
end

let map_nested : forall 'f 'g 'a 'b. functor 'f * functor 'g => ('a -> 'b) -> 'f ('g 'a) -> 'f ('g 'b) =
  fun x -> map map map x
