do
  local el = { _1 = function(x) return x end, _2 = function(x) return x end }
  (nil)({ e = el._2, d = el._1, a = 3, b = 5, c = 6 })
end
