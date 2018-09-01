do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local rem = function(x, y) return x % y end
  local bottom = nil
  bottom(rem(5, 3))
end
