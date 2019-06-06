do
  local eg = { _1 = function(x) return x end, _2 = function(x) return x end }
  (nil)({ d = eg._1, e = eg._2, a = 3, b = 5, c = 6 })
end
