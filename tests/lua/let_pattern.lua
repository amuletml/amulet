do
  local el = { _1 = function(x) return x end, _2 = function(x) return x end }
  (nil)({ d = el._1, e = el._2, a = 3, b = 5, c = 6 })
end
