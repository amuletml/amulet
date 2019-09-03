do
  local em = { _1 = function(x) return x end, _2 = function(x) return x end }
  (nil)({ d = em._1, e = em._2, a = 3, b = 5, c = 6 })
end
