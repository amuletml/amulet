do
  local ei = { _1 = function(x) return x end, _2 = function(x) return x end }
  local bottom = nil
  bottom({ a = 3, b = 5, c = 6, d = ei._1, e = ei._2 })
end
