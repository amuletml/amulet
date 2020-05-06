do
  local Nil = { __tag = "Nil" }
  local function filter(f)
    local function filter_sat(x)
      if x.__tag ~= "Cons" then return Nil end
      local tmp = x[1]
      local x0, xs = tmp._1, tmp._2
      if f(x0) then return { { _1 = x0, _2 = filter_sat(xs) }, __tag = "Cons" } end
      return filter_sat(xs)
    end
    return filter_sat
  end
  (nil)(filter)
end
