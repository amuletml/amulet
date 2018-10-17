class empty
instance empty int

class semigroup 'a
  val (<>) : 'a -> 'a -> 'a

instance semigroup int
  let (<>) = (+)

class multiline
  'a
  val foo : 'a
  val bar : 'a

instance multiline string
  => multiline int
  let foo = 1
  let bar = 2

class multi 'a 'b 'c
instance multi int string () begin end
