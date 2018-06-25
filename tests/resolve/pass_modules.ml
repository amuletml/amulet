module X =
  module Y =
    let x = 1

module Z = X.Y


let a = Z.x

open X

let b = Y.x

let z = X.{ .a = Y.x }
