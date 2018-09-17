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
          local hs = xs[1]
          if ys.__tag == "Nil" then
            return Cons({ _1 = 2, _2 = Nil })
          elseif ys.__tag == "Cons" then
            local ht = ys[1]
            local hx = ht._2
            local hv = hs._1
            local hu = hs._2
            local hy = ht._1
            if 0 == hy then
              if 0 == hv then
                return Cons({ _1 = 3, _2 = Nil })
              else
                return Cons({ _1 = f(hv)(hy), _2 = zip(f)(hu)(hx) })
              end
            else
              return Cons({ _1 = f(hv)(hy), _2 = zip(f)(hu)(hx) })
            end
          end
        end
      end
    end
  end
  local bottom = nil
  bottom(zip)
end
