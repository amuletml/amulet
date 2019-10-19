do
  local Nil = { __tag = "Nil" }
  local function zip(f, xs, ys)
    if xs.__tag == "Nil" then return { { _1 = 1, _2 = Nil }, __tag = "Cons" } end
    local tmp = xs[1]
    if ys.__tag == "Nil" then return { { _1 = 2, _2 = Nil }, __tag = "Cons" } end
    local tmp2 = ys[1]
    local tmp3, tmp4 = tmp2._1, tmp2._2
    local tmp0, tmp1 = tmp._1, tmp._2
    if tmp0 ~= 0 then return { { _1 = f(tmp0)(tmp3), _2 = zip(f, tmp1, tmp4) }, __tag = "Cons" } end
    if tmp3 == 0 then return { { _1 = 3, _2 = Nil }, __tag = "Cons" } end
    return { { _1 = f(0)(tmp3), _2 = zip(f, tmp1, tmp4) }, __tag = "Cons" }
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  (nil)(zip0)
end
