do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local zip
  zip = function(f)
    return function(xs)
      return function(ys)
        if xs.__tag == "Nil" then
          return Cons({ _1 = 1, _2 = Nil })
        elseif xs.__tag == "Cons" then
          local hh = xs[1]
          if ys.__tag == "Nil" then
            return Cons({ _1 = 2, _2 = Nil })
          elseif ys.__tag == "Cons" then
            local hi = ys[1]
            local hk = hi._1
            local hj = hi._2
            local hn = hh._1
            local hm = hh._2
            if 0 == hn then
              if 0 == hk then
                return Cons({ _1 = 3, _2 = Nil })
              else
                return Cons({ _1 = f(hn)(hk), _2 = zip(f)(hm)(hj) })
              end
            else
              return Cons({ _1 = f(hn)(hk), _2 = zip(f)(hm)(hj) })
            end
          end
        end
      end
    end
  end
  local bottom = nil
  bottom(zip)
end
