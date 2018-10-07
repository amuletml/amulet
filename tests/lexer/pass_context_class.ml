module Automatic =
  (*
  class empty
  instance empty int
  *)

  class semigroup 'a
    val (<>) : 'a -> 'a -> 'a


  instance semigroup int
    let (<>) = (+)

module Explicit =
  (*
  class empty begin
  end

  instance empty int begin
  end
  *)

  class semigroup 'a begin
    val (<>) : 'a -> 'a -> 'a
  end

  instance semigroup int begin
    let (<>) = (+)
  end
