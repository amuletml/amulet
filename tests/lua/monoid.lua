do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local __builtin_unit = { __tag = "__builtin_unit" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeafe(agw)
    return {
      ["<*>"] = function(cbi) return function(cbf) return agw["×"](cbi)(cbf) end end,
      pure = function(cbp) return agw.zero end,
      ["Applicative$kl"] = function(cau) return function(car) return car end end
    }
  end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollardShowarv(ath)
    return function(cs)
      if cs.__tag == "Nil" then
        return "Nil"
      elseif cs.__tag == "Cons" then
        local cdn = cs[1]
        return ath(cdn._1) .. " :: " .. _dollardShowarv(ath)(cdn._2)
      end
    end
  end
  local function _dollardTraversablebhm(chi)
    return function(chf)
      return function(k)
        return function(cu)
          if cu.__tag == "Nil" then
            return chf.pure(Nil)
          elseif cu.__tag == "Cons" then
            local chb = cu[1]
            return chf["<*>"](chf["Applicative$kl"](_colon_colon)(k(chb._1)))(_dollardTraversablebhm(__builtin_unit)(chf)(k)(chb._2))
          end
        end
      end
    end
  end
  local function _dollardMonoidbmc(chz)
    return {
      ["×"] = function(x)
        return function(ys)
          if x.__tag == "Nil" then
            return ys
          elseif x.__tag == "Cons" then
            local cht = x[1]
            return Cons({ _1 = cht._1, _2 = _dollardMonoidbmc(__builtin_unit)["×"](cht._2)(ys) })
          end
        end
      end,
      zero = Nil
    }
  end
  local cjb = { _1 = 1, _2 = __builtin_unit }
  writeln(_dollardShowarv(function(x)
    return tostring(x)
  end)(_dollardTraversablebhm(__builtin_unit)(_dollardApplicativeafe(_dollardMonoidbmc(__builtin_unit)))(function(ccg)
    return Cons({ _2 = Nil, _1 = ccg._1 })
  end)(Cons({ _1 = cjb, _2 = Cons({ _1 = cjb, _2 = Cons({ _1 = cjb, _2 = Nil }) }) }))))
end
