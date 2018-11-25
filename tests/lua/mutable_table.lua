do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local empty = function() return {} end
  local set = function(tbl, k, v)
    tbl[k] = v
  end
  local t = empty(__builtin_unit)
  set(t, "foo", "foo")
end
