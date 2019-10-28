let f & g = fun x -> f (g x)

class functor 'f begin
  val (<$>) : forall 'a 'b. ('a -> 'b) -> 'f 'a -> 'f 'b
end

let fmap = (<$>)

class functor 'f => applicative 'f begin
  val pure : forall 'a. 'a -> 'f 'a
  val (<*>) : forall 'a 'b. 'f ('a -> 'b) -> 'f 'a -> 'f 'b
end

class functor 't => traversable 't begin
  val traverse :
    forall 'a 'b 'f. applicative 'f =>
      ('a -> 'f 'b) -> 't 'a -> 'f ('t 'b)
  val sequence :
    forall 'a 'f. applicative 'f =>
      't ('f 'a) -> 'f ('t 'a)

  let sequence x = traverse (fun x -> x) x
  let traverse f = sequence & fmap f
end

