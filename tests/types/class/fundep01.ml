class generic 'a 'rep | 'a -> 'rep begin 
  val into : 'a -> 'rep 'k
end

type k1 'a 'x = K1 of 'a

type foo = Foo of int

instance generic foo (k1 int) begin
  let into (Foo x) = K1 x
end

class gshow 'f begin
  val gshow : 'f 'x -> string
end

class show 'a begin
  val show : 'a -> string
end

instance show int begin
  let show _ = "int"
end

instance show 'a => gshow (k1 'a) begin
  let gshow (K1 x) = show x
end

let genericShow x = gshow (into x)
