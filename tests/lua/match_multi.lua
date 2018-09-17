do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  local bl = bottom(__builtin_unit)
  bottom(bl.a + bl.b)
end
