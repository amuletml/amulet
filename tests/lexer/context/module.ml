(* Implicit begin *)
module X =
  let _ = 1

(* Explicit begin *)
module X = struct
  let _ = 1
end

(* Explicit begin with weird alignment *)
module X =
struct
  let _ = 1
end

module X =
  struct
    let _ = 1
  end

module X = Y

open struct
  type x
end

(* Access modifiers *)
private module Y =
  let a = 0
