module A = begin
  module B = begin
    let x = 1
  end
end

let 1 = A.B.( x )
