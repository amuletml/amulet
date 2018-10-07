class semigroup 'a
  val (<>) : 'a -> 'a -> 'a

instance semigroup int
  let (<>) = (+)
