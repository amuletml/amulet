do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local function f(x) return x(__builtin_unit) end
  local function g(x) return x(__builtin_unit) end
  (nil)({ f = f, g = g })
end
