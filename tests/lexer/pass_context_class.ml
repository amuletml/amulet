module Automatic =
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

module Explicit =
  class empty begin
  end

  instance empty begin
  end

  class empty'
  begin end

  instance empty'
  begin end

  class semigroup 'a begin
    val (<>) : 'a -> 'a -> 'a
  end

  instance semigroup int begin
    let (<>) = (+)
  end

  class multiline
    'a begin
    val foo : 'a ;;
    val bar : 'a
  end

  instance multiline string
    => multiline int begin
    let foo = 1 ;;
    let bar = 2
  end
