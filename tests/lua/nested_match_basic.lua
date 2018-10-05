do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function zip(ik_1, ik_2, ik_3)
    if ik_2.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    elseif ik_2.__tag == "Cons" then
      local gw = ik_2[1]
      if ik_3.__tag == "Nil" then
        return Cons({ _1 = 2, _2 = Nil })
      elseif ik_3.__tag == "Cons" then
        local gx = ik_3[1]
        return Cons({ _1 = ik_1(gw._1)(gx._1), _2 = zip(ik_1, gw._2, gx._2) })
      end
    end
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  local bottom = nil
  bottom(zip0)
end
