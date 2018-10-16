do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function zip(it_1, it_2, it_3)
    if it_2.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    elseif it_2.__tag == "Cons" then
      local hf = it_2[1]
      if it_3.__tag == "Nil" then
        return Cons({ _1 = 2, _2 = Nil })
      elseif it_3.__tag == "Cons" then
        local hg = it_3[1]
        return Cons({ _1 = it_1(hf._1)(hg._1), _2 = zip(it_1, hf._2, hg._2) })
      end
    end
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  local bottom = nil
  bottom(zip0)
end
