do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  local bh = bottom(__builtin_unit)
  bottom(bh.a + bh.b)
end
