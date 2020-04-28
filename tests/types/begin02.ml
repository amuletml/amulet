class monad 'm begin
  val pure : 'a -> 'm 'a
  val (>>=) : 'm 'a -> ('a -> 'm 'b) -> 'm 'b
end

type identity 'a = Identity of 'a

instance monad identity begin
  let pure = Identity
  let (Identity x) >>= f = f x
end

let test =
  let! x = Identity 1
  pure 2
