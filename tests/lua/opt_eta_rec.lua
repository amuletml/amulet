do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local function f(x) return f(x) end
  f(__builtin_unit)
end
