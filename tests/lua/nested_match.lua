do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local function zip(f, xs, ys)
    if xs.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    elseif xs.__tag == "Cons" then
      local fx = xs[1]
      if ys.__tag == "Nil" then
        return Cons({ _1 = 2, _2 = Nil })
      elseif ys.__tag == "Cons" then
        local fy = ys[1]
        local ga, fz = fy._1, fy._2
        local gd, gc = fx._1, fx._2
        if gd == 0 then
          if ga == 0 then
            return Cons({ _1 = 3, _2 = Nil })
          else
            return Cons({ _1 = f(0)(ga), _2 = zip(f, gc, fz) })
          end
        else
          return Cons({ _1 = f(gd)(ga), _2 = zip(f, gc, fz) })
        end
      end
    end
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  (nil)(zip0)
end
