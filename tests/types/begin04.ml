class monad 'm begin
  val pure : 'a -> 'm 'a
  val (>>=) : 'm 'a -> ('a -> 'm 'b) -> 'm 'b
end

type identity 'a = Identity of 'a

instance monad identity begin
  let pure = Identity
  let (Identity x) >>= f = f x
end

instance monad list begin
  let pure x = [x]
  let xs >>= f = [ x | with x <- xs, with x <- f x ]
end

(* can't mix and match monads *)
let test =
  let! x = Identity 1
  let! y = [2]
  pure 2