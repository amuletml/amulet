class c2 't begin
  type f2
end

instance c2 int begin
  type f2 = bool
end

external val bot : 'a = "nil"

let fst (x, _) = x
let foo x = fst (x, bot : f2 bool)
