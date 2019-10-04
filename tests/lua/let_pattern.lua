do
  local et = { _1 = function(x) return x end, _2 = function(x) return x end }
  (nil)({ d = et._1, e = et._2, a = 3, b = 5, c = 6 })
end
