do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local dm = { _1 = function(x) return x end, _2 = function(x) return x end }
  local d = dm._1
  local e = dm._2
  local bottom = nil
  bottom({ a = 3, b = 5, c = 6, d = d, e = e })
end
