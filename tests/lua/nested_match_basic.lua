do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function zip(f, xs, ys)
    if xs.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    elseif xs.__tag == "Cons" then
      local hi = xs[1]
      if ys.__tag == "Nil" then
        return Cons({ _1 = 2, _2 = Nil })
      elseif ys.__tag == "Cons" then
        local hj = ys[1]
        return Cons({ _1 = f(hi._1)(hj._1), _2 = zip(f, hi._2, hj._2) })
      end
    end
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  local bottom = nil
  bottom(zip0)
end
