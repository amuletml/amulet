do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local function zip(f, xs, ys)
    if xs.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    end
    local fx = xs[1]
    if ys.__tag == "Nil" then
      return Cons({ _1 = 2, _2 = Nil })
    end
    local fy = ys[1]
    return Cons({ _1 = f(fx._1)(fy._1), _2 = zip(f, fx._2, fy._2) })
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  (nil)(zip0)
end
