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

let test = begin
  with x <- Identity 1
  pure 2
end

let tes2 = begin
  with x <- [1]
  pure 2
end
