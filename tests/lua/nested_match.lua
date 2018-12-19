do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local function zip(f, xs, ys)
    if xs.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    elseif xs.__tag == "Cons" then
      local fw = xs[1]
      if ys.__tag == "Nil" then
        return Cons({ _1 = 2, _2 = Nil })
      elseif ys.__tag == "Cons" then
        local fx = ys[1]
        local gc, gb = fx._1, fx._2
        local fz, fy = fw._1, fw._2
        if gc == 0 then
          if fz == 0 then
            return Cons({ _1 = 3, _2 = Nil })
          else
            return Cons({ _1 = f(fz)(0), _2 = zip(f, fy, gb) })
          end
        else
          return Cons({ _1 = f(fz)(gc), _2 = zip(f, fy, gb) })
        end
      end
    end
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  (nil)(zip0)
end
