do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  local bo = bottom(__builtin_unit)
  bottom(bo.a + bo.b)
end
