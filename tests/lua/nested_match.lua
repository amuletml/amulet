do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function zip(kg_1, kg_2, kg_3)
    if kg_2.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    elseif kg_2.__tag == "Cons" then
      local hq = kg_2[1]
      if kg_3.__tag == "Nil" then
        return Cons({ _1 = 2, _2 = Nil })
      elseif kg_3.__tag == "Cons" then
        local ht = hq._1
        local hs = hq._2
        local hr = kg_3[1]
        local hw = hr._1
        local hv = hr._2
        if 0 == hw then
          if 0 == ht then
            return Cons({ _1 = 3, _2 = Nil })
          else
            return Cons({ _1 = kg_1(ht)(0), _2 = zip(kg_1, hs, hv) })
          end
        else
          return Cons({ _1 = kg_1(ht)(hw), _2 = zip(kg_1, hs, hv) })
        end
      end
    end
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  local bottom = nil
  bottom(zip0)
end
