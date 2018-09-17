do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  bottom(function(bn) return (bn.a + bn.b) * bn.c end)
end
