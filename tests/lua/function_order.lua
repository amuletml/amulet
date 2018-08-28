do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  local a = bottom(1)
  local b = bottom(2)
  local c = bottom(3)
  bottom(b)(c)(a)
end
