do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local of_any = function(a) return a end
  local empty = function() return {} end
  local get = function(tbl, k) return tbl[k] end
  local set = function(tbl, k, v)
    tbl[k] = v
  end
  local t = empty(__builtin_unit)
  set(t, "foo", of_any("foo"))
  get(t, "foo")
end
