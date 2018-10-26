do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function zip(f, xs, ys)
    if xs.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    elseif xs.__tag == "Cons" then
      local hr = xs[1]
      if ys.__tag == "Nil" then
        return Cons({ _1 = 2, _2 = Nil })
      elseif ys.__tag == "Cons" then
        local hu, ht = hr._1, hr._2
        local hs = ys[1]
        local hx, hw = hs._1, hs._2
        if hu == 0 then
          if hx == 0 then
            return Cons({ _1 = 3, _2 = Nil })
          else
            return Cons({ _1 = f(0)(hx), _2 = zip(f, ht, hw) })
          end
        else
          return Cons({ _1 = f(hu)(hx), _2 = zip(f, ht, hw) })
        end
      end
    end
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  local bottom = nil
  bottom(zip0)
end
