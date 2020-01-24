external val (+) : int -> int -> int =
  "function(x, y) return x + y end"

module X = struct
  type t = T
  let x = T
end

let _ =
  let _ = X.x + 1
  (* local opens unqualify *)
  let _ =
    let open X
    x + 1
  let _ = X.( x + 1 )

  (* make sure that the unqualified scope doesn't leak *)
  let _ = X.x + 1
  ()
