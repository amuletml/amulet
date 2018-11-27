do
  local el = { _1 = function(x) return x end, _2 = function(x) return x end }
  (nil)({ a = 3, b = 5, c = 6, d = el._1, e = el._2 })
end
