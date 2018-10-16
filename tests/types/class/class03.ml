class eq 'a begin
  val eq : 'a -> 'a -> bool
end

class eq 'a => ord 'a begin
  val lte : 'a -> 'a -> bool
end
