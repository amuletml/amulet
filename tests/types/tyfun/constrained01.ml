class c2 't begin
  type f2
  val into : 't -> f2 't
end

instance c2 int begin
  type f2 = bool
  let into _ = true
end

external val bot : 'a = "nil"

let fst (x, _) = x

let foo x = fst (x, into x)
