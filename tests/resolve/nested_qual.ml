module A = struct
  module B = struct
    let x = 1
  end
end

let 1 = A.B.( x )
