do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local f
  f = function(x) return f(x) + 1 + 1 end
  local bottom = nil
  bottom(f)
end
