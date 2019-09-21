do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local function zip(f, xs, ys)
    if xs.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    end
    local ge = xs[1]
    if ys.__tag == "Nil" then
      return Cons({ _1 = 2, _2 = Nil })
    end
    local gf = ys[1]
    local gj, gi = gf._1, gf._2
    local gh, gg = ge._1, ge._2
    if gh ~= 0 then
      return Cons({ _1 = f(gh)(gj), _2 = zip(f, gg, gi) })
    end
    if gj == 0 then
      return Cons({ _1 = 3, _2 = Nil })
    end
    return Cons({ _1 = f(0)(gj), _2 = zip(f, gg, gi) })
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  (nil)(zip0)
end
