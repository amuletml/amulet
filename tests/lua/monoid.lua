do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local __builtin_unit = { __tag = "__builtin_unit" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeafi(cdf)
    return {
      pure = function(ccr) return cdf.zero end,
      ["<*>"] = function(cci) return function(ccf) return cdf["×"](cci)(ccf) end end,
      ["Applicative$kl"] = function(cbq) return function(cbn) return cbn end end
    }
  end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollarshow(atm, cs)
    if cs.__tag == "Nil" then
      return "Nil"
    elseif cs.__tag == "Cons" then
      local cfc = cs[1]
      return atm(cfc._1) .. " :: " .. _dollarshow(atm, cfc._2)
    end
  end
  local function _dollartraverse(bip, cji, k, cu)
    if cu.__tag == "Nil" then
      return cji.pure(Nil)
    elseif cu.__tag == "Cons" then
      local cje = cu[1]
      return cji["<*>"](cji["Applicative$kl"](_colon_colon)(k(cje._1)))(_dollartraverse(__builtin_unit, cji, k, cje._2))
    end
  end
  local function _dollar_d7(bnn, x, ys)
    if x.__tag == "Nil" then
      return ys
    elseif x.__tag == "Cons" then
      local ckf = x[1]
      return Cons({ _1 = ckf._1, _2 = _dollar_d7(__builtin_unit, ckf._2, ys) })
    end
  end
  local cll = { _1 = 1, _2 = __builtin_unit }
  writeln(_dollarshow(function(x)
    return tostring(x)
  end, _dollartraverse(__builtin_unit, _dollardApplicativeafi({
    zero = Nil,
    ["×"] = function(x) return function(ys) return _dollar_d7(__builtin_unit, x, ys) end end
  }), function(cdq) return Cons({ _1 = cdq._1, _2 = Nil }) end, Cons({
    _1 = cll,
    _2 = Cons({ _1 = cll, _2 = Cons({ _1 = cll, _2 = Nil }) })
  }))))
end
