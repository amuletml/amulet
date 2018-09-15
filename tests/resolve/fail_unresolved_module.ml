(* MIssing modules *)
module X =
  module X.X =
    let a = 0
  module Y =
    let a = 0

open X.Z


