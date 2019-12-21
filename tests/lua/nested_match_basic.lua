do
  local Nil = { __tag = "Nil" }
  local function zip(f)
    local function zip_sat(xs, ys)
      if xs.__tag == "Nil" then return { { _1 = 1, _2 = Nil }, __tag = "Cons" } end
      local tmp = xs[1]
      if ys.__tag == "Nil" then return { { _1 = 2, _2 = Nil }, __tag = "Cons" } end
      local tmp0 = ys[1]
      return { { _1 = f(tmp._1)(tmp0._1), _2 = zip_sat(tmp._2, tmp0._2) }, __tag = "Cons" }
    end
    return function(xs) return function(ys) return zip_sat(xs, ys) end end
  end
  (nil)(zip)
end
