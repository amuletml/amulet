do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local _rem = function(x, y) return x % y end
  local rem = function(a) return function(b) return _rem(a, b) end end
  local bottom = nil
  bottom(rem(5)(3))
end
