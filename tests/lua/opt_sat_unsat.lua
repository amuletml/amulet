do
  local Nil = { __tag = "Nil" }
  local function map(f)
    local function map_sat(x)
      if x.__tag == "Nil" then return Nil end
      (function(ji) return function(x) return map_sat(x) end end)(function(x0) return x0 end)({
        { _1 = 1, _2 = Nil },
        __tag = "Cons"
      })
      local tmp = x[1]
      return { { _1 = f(tmp._1), _2 = map_sat(tmp._2) }, __tag = "Cons" }
    end
    return map_sat
  end
end
