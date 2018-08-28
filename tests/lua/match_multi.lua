do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  local bc = bottom(__builtin_unit)
  bottom(bc.a + bc.b)
end
