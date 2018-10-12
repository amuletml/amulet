do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function zip(iq_1, iq_2, iq_3)
    if iq_2.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    elseif iq_2.__tag == "Cons" then
      local hc = iq_2[1]
      if iq_3.__tag == "Nil" then
        return Cons({ _1 = 2, _2 = Nil })
      elseif iq_3.__tag == "Cons" then
        local hd = iq_3[1]
        return Cons({ _1 = iq_1(hc._1)(hd._1), _2 = zip(iq_1, hc._2, hd._2) })
      end
    end
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  local bottom = nil
  bottom(zip0)
end
