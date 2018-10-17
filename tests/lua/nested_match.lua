do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function zip(kh_1, kh_2, kh_3)
    if kh_2.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    elseif kh_2.__tag == "Cons" then
      local hr = kh_2[1]
      if kh_3.__tag == "Nil" then
        return Cons({ _1 = 2, _2 = Nil })
      elseif kh_3.__tag == "Cons" then
        local ht = hr._2
        local hs = kh_3[1]
        local hx = hs._1
        local hw = hs._2
        local hu = hr._1
        if 0 == hu then
          if 0 == hx then
            return Cons({ _1 = 3, _2 = Nil })
          else
            return Cons({ _1 = kh_1(0)(hx), _2 = zip(kh_1, ht, hw) })
          end
        else
          return Cons({ _1 = kh_1(hu)(hx), _2 = zip(kh_1, ht, hw) })
        end
      end
    end
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  local bottom = nil
  bottom(zip0)
end
