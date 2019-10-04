do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local function zip(f, xs, ys)
    if xs.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    end
    local gj = xs[1]
    if ys.__tag == "Nil" then
      return Cons({ _1 = 2, _2 = Nil })
    end
    local gk = ys[1]
    local go, gn = gk._1, gk._2
    local gm, gl = gj._1, gj._2
    if go ~= 0 then
      return Cons({ _1 = f(gm)(go), _2 = zip(f, gl, gn) })
    end
    if gm == 0 then
      return Cons({ _1 = 3, _2 = Nil })
    end
    return Cons({ _1 = f(gm)(0), _2 = zip(f, gl, gn) })
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  (nil)(zip0)
end
