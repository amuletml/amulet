do
  local tmp = { _1 = function(x) return x end, _2 = function(x) return x end };
  (nil)({ e = tmp._2, d = tmp._1, a = 3, b = 5, c = 6 })
end
