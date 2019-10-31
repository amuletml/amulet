let rec x + y = x + y

module X =
  private module Y =
    let a = 0

  let private b = Y.a

  private type c = A | B

  private class d

let () = (X.Y.a + X.b + X.A) : X.c

instance X.d int begin end
