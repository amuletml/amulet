let rec x + y = x + y

class semigroup 'a
  val (<>) : 'a -> 'a -> 'a

instance semigroup int
  let (<>) = (+)

class default 'a
  val a : 'a
  val b : 'a

  let a = b
