do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeajd(cka)
    return {
      ["<*>"] = function(cjd) return function(cja) return cka["×"](cjd)(cja) end end,
      pure = function(cjm) return cka.zero end,
      ["Applicative$ma"] = function(cil) return function(cii) return cii end end
    }
  end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollarshow(aym, cs)
    if cs.__tag == "Nil" then
      return "Nil"
    end
    local clv = cs[1]
    return aym(clv._1) .. " :: " .. _dollarshow(aym, clv._2)
  end
  local function _dollartraverse(bpb, cpv, k, cu)
    if cu.__tag == "Nil" then
      return cpv.pure(Nil)
    end
    local cps = cu[1]
    return cpv["<*>"](cpv["Applicative$ma"](_colon_colon)(k(cps._1)))(_dollartraverse(nil, cpv, k, cps._2))
  end
  local function _dollar_d7(bua, x, ys)
    if x.__tag == "Nil" then
      return ys
    end
    local cqs = x[1]
    return Cons({ _2 = _dollar_d7(nil, cqs._2, ys), _1 = cqs._1 })
  end
  local crx = { _1 = 1, _2 = nil }
  writeln(_dollarshow(function(x)
    return tostring(x)
  end, _dollartraverse(nil, _dollardApplicativeajd({
    ["×"] = function(x) return function(ys) return _dollar_d7(nil, x, ys) end end,
    zero = Nil
  }), function(ckl) return Cons({ _1 = ckl._1, _2 = Nil }) end, Cons({
    _1 = crx,
    _2 = Cons({ _1 = crx, _2 = Cons({ _1 = crx, _2 = Nil }) })
  }))))
end
