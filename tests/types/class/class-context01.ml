class eq 'a begin
  val eq : 'a -> 'a -> bool
end

let foo (x, y) (a, b) =
  if eq x y then eq a b else false
