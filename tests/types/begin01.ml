class monad 'm begin
  val pure : 'a -> 'm 'a
  val (>>=) : 'm 'a -> ('a -> 'm 'b) -> 'm 'b
end

type identity 'a = Identity of 'a

let _ =
  let! x = Identity 1
  pure 2
