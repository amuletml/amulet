module X =
  module private Y =
    let a = 0

  let private b = Y.a

  type private c = A | B

  class private d

let () = (X.Y.a + X.b + X.A) : X.c

instance X.d int begin end
