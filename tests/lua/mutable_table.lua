do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local empty = function() return {} end
  local t = empty(__builtin_unit)
  t.foo = "foo"
end
