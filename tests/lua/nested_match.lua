do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function zip(kd_1, kd_2, kd_3)
    if kd_2.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    elseif kd_2.__tag == "Cons" then
      local hn = kd_2[1]
      if kd_3.__tag == "Nil" then
        return Cons({ _1 = 2, _2 = Nil })
      elseif kd_3.__tag == "Cons" then
        local ho = kd_3[1]
        local ht = ho._1
        local hs = ho._2
        local hq = hn._1
        local hp = hn._2
        if 0 == hq then
          if 0 == ht then
            return Cons({ _1 = 3, _2 = Nil })
          else
            return Cons({ _1 = kd_1(0)(ht), _2 = zip(kd_1, hp, hs) })
          end
        else
          return Cons({ _1 = kd_1(hq)(ht), _2 = zip(kd_1, hp, hs) })
        end
      end
    end
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  local bottom = nil
  bottom(zip0)
end
