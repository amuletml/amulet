do
  local ek = { _1 = function(x) return x end, _2 = function(x) return x end }
  (nil)({ d = ek._1, e = ek._2, a = 3, b = 5, c = 6 })
end
