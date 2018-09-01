do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  local bg = bottom(__builtin_unit)
  bottom(bg.a + bg.b)
end
