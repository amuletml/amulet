do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function zip(jb_1, jb_2, jb_3)
    if jb_2.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    elseif jb_2.__tag == "Cons" then
      local hr = jb_2[1]
      if jb_3.__tag == "Nil" then
        return Cons({ _1 = 2, _2 = Nil })
      elseif jb_3.__tag == "Cons" then
        local ht = hr._2
        local hs = jb_3[1]
        local hx = hs._1
        local hw = hs._2
        local hu = hr._1
        if 0 == hu then
          if 0 == hx then
            return Cons({ _1 = 3, _2 = Nil })
          else
            return Cons({ _1 = jb_1(0)(hx), _2 = zip(jb_1, ht, hw) })
          end
        else
          return Cons({ _1 = jb_1(hu)(hx), _2 = zip(jb_1, ht, hw) })
        end
      end
    end
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  local bottom = nil
  bottom(zip0)
end
