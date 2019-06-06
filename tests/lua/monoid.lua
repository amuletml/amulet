do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local __builtin_unit = { __tag = "__builtin_unit" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeaew(cba)
    return {
      pure = function(cam) return cba.zero end,
      ["<*>"] = function(cad) return function(caa) return cba["×"](cad)(caa) end end,
      ["Applicative$kl"] = function(cal) return function(cai) return cai end end
    }
  end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollarshow(ass, cs)
    if cs.__tag == "Nil" then
      return "Nil"
    elseif cs.__tag == "Cons" then
      local ccv = cs[1]
      return ass(ccv._1) .. " :: " .. _dollarshow(ass, ccv._2)
    end
  end
  local function _dollartraverse(bhn, cgv, k, cu)
    if cu.__tag == "Nil" then
      return cgv.pure(Nil)
    elseif cu.__tag == "Cons" then
      local cgs = cu[1]
      return cgv["<*>"](cgv["Applicative$kl"](_colon_colon)(k(cgs._1)))(_dollartraverse(__builtin_unit, cgv, k, cgs._2))
    end
  end
  local function _dollar_d7(blz, x, ys)
    if x.__tag == "Nil" then
      return ys
    elseif x.__tag == "Cons" then
      local chs = x[1]
      return Cons({ _1 = chs._1, _2 = _dollar_d7(__builtin_unit, chs._2, ys) })
    end
  end
  local cix = { _1 = 1, _2 = __builtin_unit }
  writeln(_dollarshow(function(x)
    return tostring(x)
  end, _dollartraverse(__builtin_unit, _dollardApplicativeaew({
    zero = Nil,
    ["×"] = function(x) return function(ys) return _dollar_d7(__builtin_unit, x, ys) end end
  }), function(cbl) return Cons({ _1 = cbl._1, _2 = Nil }) end, Cons({
    _1 = cix,
    _2 = Cons({ _1 = cix, _2 = Cons({ _1 = cix, _2 = Nil }) })
  }))))
end
