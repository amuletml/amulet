do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeafj(ahb)
    return {
      ["<*>"] = function(cdj) return function(cdg) return ahb["×"](cdj)(cdg) end end,
      pure = function(cdq) return ahb.zero end,
      ["Applicative$kp"] = _dollardApplicativeafj(ahb)["Applicative$kp"]
    }
  end
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollardShowatm(auy)
    return function(cw)
      if cw.__tag == "Cons" then
        local cfq = cw[1]
        return auy(cfq._1) .. " :: " .. _dollardShowatm(auy)(cfq._2)
      elseif cw.__tag == "Nil" then
        return "Nil"
      end
    end
  end
  local function _dollardTraversablebjd(cjl)
    return function(cji)
      return function(k)
        return function(cy)
          if cy.__tag == "Cons" then
            local cje = cy[1]
            return cji["<*>"](cji["Applicative$kp"](_colon_colon)(k(cje._1)))(_dollardTraversablebjd(__builtin_unit)(cji)(k)(cje._2))
          elseif cy.__tag == "Nil" then
            return cji.pure(Nil)
          end
        end
      end
    end
  end
  local function _dollardMonoidbnt(ckc)
    return {
      zero = Nil,
      ["×"] = function(x)
        return function(ys)
          if x.__tag == "Cons" then
            local cjw = x[1]
            return Cons({ _1 = cjw._1, _2 = _dollardMonoidbnt(__builtin_unit)["×"](cjw._2)(ys) })
          elseif x.__tag == "Nil" then
            return ys
          end
        end
      end
    }
  end
  local cmn = _dollardTraversablebjd(__builtin_unit)(_dollardApplicativeafj(_dollardMonoidbnt(__builtin_unit)))
  local cno = _dollardApplicativeafj(_dollardMonoidbnt(__builtin_unit))
  local cap = cno["Applicative$kp"]
  local cle = { _1 = 1, _2 = __builtin_unit }
  writeln(_dollardShowatm(function(x) return tostring(x) end)(cmn(function(cej)
    local b = cej._2
    return cap(function(cu) return { _1 = cu, _2 = b } end)(Cons({ _1 = cej._1, _2 = Nil }))
  end)(Cons({ _1 = cle, _2 = Cons({ _1 = cle, _2 = Cons({ _1 = cle, _2 = Nil }) }) }))))
end
