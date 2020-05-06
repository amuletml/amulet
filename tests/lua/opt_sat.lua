do
  local Nil = { __tag = "Nil" }
  local use = print
  local function map(f)
    local function map_sat(x)
      if x.__tag ~= "Cons" then return Nil end
      local tmp = x[1]
      return { { _1 = f(tmp._1), _2 = map_sat(tmp._2) }, __tag = "Cons" }
    end
    return map_sat
  end
  use(map)
  local function map_no_sat(f, x)
    if x.__tag ~= "Cons" then return Nil end
    local tmp = x[1]
    map_no_sat(nil, Nil)
    return { { _1 = f(tmp._1), _2 = map(f)(tmp._2) }, __tag = "Cons" }
  end
  local function map_no_sat0(f) return function(x) return map_no_sat(f, x) end end
  use(map_no_sat0)
end
