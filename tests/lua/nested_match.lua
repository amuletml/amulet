do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local function zip(f, xs, ys)
    if xs.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    elseif xs.__tag == "Cons" then
      local ft = xs[1]
      if ys.__tag == "Nil" then
        return Cons({ _1 = 2, _2 = Nil })
      elseif ys.__tag == "Cons" then
        local fw, fv = ft._1, ft._2
        local fu = ys[1]
        local fz, fy = fu._1, fu._2
        if fz == 0 then
          if fw == 0 then
            return Cons({ _1 = 3, _2 = Nil })
          else
            return Cons({ _1 = f(fw)(0), _2 = zip(f, fv, fy) })
          end
        else
          return Cons({ _1 = f(fw)(fz), _2 = zip(f, fv, fy) })
        end
      end
    end
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  (nil)(zip0)
end
