do
  local Nil = { __tag = "Nil" }
  local use = print
  local function map(f)
    local function map_sat(x)
      if x.__tag == "Nil" then return Nil end
      local tmp = x[1]
      return { { _1 = f(tmp._1), _2 = map_sat(tmp._2) }, __tag = "Cons" }
    end
    return map_sat
  end
  use(map)
  local function map_no_sat_sat(f, x)
    if x.__tag == "Nil" then return Nil end
    local tmp = x[1]
    map_no_sat_sat(nil, Nil)
    local function map_sat(x0)
      if x0.__tag == "Nil" then return Nil end
      local tmp0 = x0[1]
      return { { _1 = (nil)(tmp0._1), _2 = map_sat(tmp0._2) }, __tag = "Cons" }
    end
    return { { _1 = (nil)(tmp._1), _2 = map_sat(tmp._2) }, __tag = "Cons" }
  end
  local function map_no_sat(f) return function(x) return map_no_sat_sat(f, x) end end
  use(map_no_sat)
end
