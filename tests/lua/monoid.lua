do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local __builtin_unit = { __tag = "__builtin_unit" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeaff(agx)
    return {
      pure = function(cbs) return agx.zero end,
      ["<*>"] = function(cbl) return function(cbi) return agx["×"](cbl)(cbi) end end,
      ["Applicative$kl"] = function(cax) return function(cau) return cau end end
    }
  end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollardShowary(atk)
    return function(cs)
      if cs.__tag == "Nil" then
        return "Nil"
      elseif cs.__tag == "Cons" then
        local cdq = cs[1]
        return atk(cdq._1) .. " :: " .. _dollardShowary(atk)(cdq._2)
      end
    end
  end
  local function _dollardTraversablebhp(chl)
    return function(chi)
      return function(k)
        return function(cu)
          if cu.__tag == "Nil" then
            return chi.pure(Nil)
          elseif cu.__tag == "Cons" then
            local che = cu[1]
            return chi["<*>"](chi["Applicative$kl"](_colon_colon)(k(che._1)))(_dollardTraversablebhp(__builtin_unit)(chi)(k)(che._2))
          end
        end
      end
    end
  end
  local function _dollardMonoidbmf(cic)
    return {
      ["×"] = function(x)
        return function(ys)
          if x.__tag == "Nil" then
            return ys
          elseif x.__tag == "Cons" then
            local chw = x[1]
            return Cons({ _2 = _dollardMonoidbmf(__builtin_unit)["×"](chw._2)(ys), _1 = chw._1 })
          end
        end
      end,
      zero = Nil
    }
  end
  local cje = { _1 = 1, _2 = __builtin_unit }
  writeln(_dollardShowary(function(x)
    return tostring(x)
  end)(_dollardTraversablebhp(__builtin_unit)(_dollardApplicativeaff(_dollardMonoidbmf(__builtin_unit)))(function(ccj)
    return Cons({ _1 = ccj._1, _2 = Nil })
  end)(Cons({ _1 = cje, _2 = Cons({ _1 = cje, _2 = Cons({ _1 = cje, _2 = Nil }) }) }))))
end
