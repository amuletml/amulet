do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function zip(jx_1, jx_2, jx_3)
    if jx_2.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    elseif jx_2.__tag == "Cons" then
      local hh = jx_2[1]
      if jx_3.__tag == "Nil" then
        return Cons({ _1 = 2, _2 = Nil })
      elseif jx_3.__tag == "Cons" then
        local hi = jx_3[1]
        local hk = hi._1
        local hj = hi._2
        local hn = hh._1
        local hm = hh._2
        if 0 == hn then
          if 0 == hk then
            return Cons({ _1 = 3, _2 = Nil })
          else
            return Cons({ _1 = jx_1(0)(hk), _2 = zip(jx_1, hm, hj) })
          end
        else
          return Cons({ _1 = jx_1(hn)(hk), _2 = zip(jx_1, hm, hj) })
        end
      end
    end
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  local bottom = nil
  bottom(zip0)
end
