do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function zip(iu_1, iu_2, iu_3)
    if iu_2.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    elseif iu_2.__tag == "Cons" then
      local hg = iu_2[1]
      if iu_3.__tag == "Nil" then
        return Cons({ _1 = 2, _2 = Nil })
      elseif iu_3.__tag == "Cons" then
        local hh = iu_3[1]
        return Cons({ _1 = iu_1(hg._1)(hh._1), _2 = zip(iu_1, hg._2, hh._2) })
      end
    end
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  local bottom = nil
  bottom(zip0)
end
