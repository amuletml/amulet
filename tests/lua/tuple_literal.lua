do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  local a = bottom(1)
  bottom({ _1 = bottom(2), _2 = { _1 = bottom(3), _2 = a } })
end
