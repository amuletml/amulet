do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  local bm = bottom(__builtin_unit)
  bottom(bm.a + bm.b)
end
