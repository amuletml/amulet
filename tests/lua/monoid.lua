do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local __builtin_unit = { __tag = "__builtin_unit" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeaew(cbf)
    return {
      pure = function(car) return cbf.zero end,
      ["<*>"] = function(cai) return function(caf) return cbf["×"](cai)(caf) end end,
      ["Applicative$kl"] = function(caq) return function(can) return can end end
    }
  end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollarshow(ass, cs)
    if cs.__tag == "Nil" then
      return "Nil"
    elseif cs.__tag == "Cons" then
      local cdc = cs[1]
      return ass(cdc._1) .. " :: " .. _dollarshow(ass, cdc._2)
    end
  end
  local function _dollartraverse(bhn, chi, k, cu)
    if cu.__tag == "Nil" then
      return chi.pure(Nil)
    elseif cu.__tag == "Cons" then
      local che = cu[1]
      return chi["<*>"](chi["Applicative$kl"](_colon_colon)(k(che._1)))(_dollartraverse(__builtin_unit, chi, k, che._2))
    end
  end
  local function _dollar_d7(blz, x, ys)
    if x.__tag == "Nil" then
      return ys
    elseif x.__tag == "Cons" then
      local cif = x[1]
      return Cons({ _2 = _dollar_d7(__builtin_unit, cif._2, ys), _1 = cif._1 })
    end
  end
  local cjl = { _1 = 1, _2 = __builtin_unit }
  writeln(_dollarshow(function(x)
    return tostring(x)
  end, _dollartraverse(__builtin_unit, _dollardApplicativeaew({
    ["×"] = function(x) return function(ys) return _dollar_d7(__builtin_unit, x, ys) end end,
    zero = Nil
  }), function(cbq) return Cons({ _1 = cbq._1, _2 = Nil }) end, Cons({
    _1 = cjl,
    _2 = Cons({ _1 = cjl, _2 = Cons({ _1 = cjl, _2 = Nil }) })
  }))))
end
