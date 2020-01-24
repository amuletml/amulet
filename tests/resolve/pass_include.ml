module X = struct
  let x = 1
end

module Y = struct
  let y = 1
  include X
end

external val (+) : int -> int -> int = "function(x, y) return x + y end"
let _ = Y.x + Y.y
