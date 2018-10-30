type nat = Z | S of nat
type vect 'n 'a =
  | Nil : vect Z 'a
  | Cons : 'a * vect 'k 'a -> vect (S 'k) 'a

class functor 'f begin
  val map : forall 'a 'b. ('a -> 'b) -> 'f 'a -> 'f 'b
end

instance functor (vect 'n) begin
  let map f = function
    | Nil -> Nil
    | Cons (x, xs) -> Cons (f x, map f xs)
end
