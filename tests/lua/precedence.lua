do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  bottom(function(bm) return (bm.a + bm.b) * bm.c end)
end
