class c2 't begin
  type f2
end

instance c2 int begin
  type f2 = bool
end

class c2 'a => c3 'a begin
  val foo : 'a -> f2 'a
end
