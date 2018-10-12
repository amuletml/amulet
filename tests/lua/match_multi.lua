do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  local bk = bottom(__builtin_unit)
  bottom(bk.a + bk.b)
end
