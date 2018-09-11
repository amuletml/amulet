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
          local hd = xs[1]
          if ys.__tag == "Nil" then
            return Cons({ _1 = 2, _2 = Nil })
          elseif ys.__tag == "Cons" then
            local he = ys[1]
            local hj = he._1
            local hi = he._2
            local hg = hd._1
            local hf = hd._2
            if 0 == hg then
              if 0 == hj then
                return Cons({ _1 = 3, _2 = Nil })
              else
                return Cons({ _1 = f(hg)(hj), _2 = zip(f)(hf)(hi) })
              end
            else
              return Cons({ _1 = f(hg)(hj), _2 = zip(f)(hf)(hi) })
            end
          end
        end
      end
    end
  end
  local bottom = nil
  bottom(zip)
end
