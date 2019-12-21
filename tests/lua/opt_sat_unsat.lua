do
  local Nil = { __tag = "Nil" }
  local map, map0
  map = function(f, x)
    if x.__tag == "Nil" then return Nil end
    map0(function(x0) return x0 end)({ { _1 = 1, _2 = Nil }, __tag = "Cons" })
    local tmp = x[1]
    return { { _1 = f(tmp._1), _2 = map(f, tmp._2) }, __tag = "Cons" }
  end
  map0 = function(f) return function(x) return map(f, x) end end
end
