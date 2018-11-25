do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function zip(f, xs, ys)
    if xs.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    elseif xs.__tag == "Cons" then
      local ht = xs[1]
      if ys.__tag == "Nil" then
        return Cons({ _1 = 2, _2 = Nil })
      elseif ys.__tag == "Cons" then
        local hu = ys[1]
        local hw, hv = hu._1, hu._2
        local hz, hy = ht._1, ht._2
        if hz == 0 then
          if hw == 0 then
            return Cons({ _1 = 3, _2 = Nil })
          else
            return Cons({ _1 = f(0)(hw), _2 = zip(f, hy, hv) })
          end
        else
          return Cons({ _1 = f(hz)(hw), _2 = zip(f, hy, hv) })
        end
      end
    end
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  local bottom = nil
  (nil)(zip0)
end
