do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local dj = { _1 = function(x) return x end, _2 = function(x) return x end }
  local d = dj._1
  local e = dj._2
  local bottom = nil
  bottom({ a = 3, b = 5, c = 6, d = d, e = e })
end
