do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local function zip(f, xs, ys)
    if xs.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    end
    local gd = xs[1]
    if ys.__tag == "Nil" then
      return Cons({ _1 = 2, _2 = Nil })
    end
    local gg, gf = gd._1, gd._2
    local ge = ys[1]
    local gi, gh = ge._1, ge._2
    if gg ~= 0 then
      return Cons({ _1 = f(gg)(gi), _2 = zip(f, gf, gh) })
    end
    if gi == 0 then
      return Cons({ _1 = 3, _2 = Nil })
    end
    return Cons({ _1 = f(0)(gi), _2 = zip(f, gf, gh) })
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  (nil)(zip0)
end
