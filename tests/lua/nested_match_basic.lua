do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local function zip(f, xs, ys)
    if xs.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    end
    local tmp = xs[1]
    if ys.__tag == "Nil" then
      return Cons({ _1 = 2, _2 = Nil })
    end
    local tmp0 = ys[1]
    return Cons({ _1 = f(tmp._1)(tmp0._1), _2 = zip(f, tmp._2, tmp0._2) })
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  (nil)(zip0)
end
