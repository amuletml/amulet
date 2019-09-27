do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeajd(cjy)
    return {
      pure = function(cjk) return cjy.zero end,
      ["<*>"] = function(cjb) return function(ciy) return cjy["×"](cjb)(ciy) end end,
      ["Applicative$ma"] = function(cij) return function(cig) return cig end end
    }
  end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollarshow(ayl, cs)
    if cs.__tag == "Nil" then
      return "Nil"
    end
    local clt = cs[1]
    return ayl(clt._1) .. " :: " .. _dollarshow(ayl, clt._2)
  end
  local function _dollartraverse(boz, cpt, k, cu)
    if cu.__tag == "Nil" then
      return cpt.pure(Nil)
    end
    local cpq = cu[1]
    return cpt["<*>"](cpt["Applicative$ma"](_colon_colon)(k(cpq._1)))(_dollartraverse(nil, cpt, k, cpq._2))
  end
  local function _dollar_d7(bty, x, ys)
    if x.__tag == "Nil" then
      return ys
    end
    local cqq = x[1]
    return Cons({ _2 = _dollar_d7(nil, cqq._2, ys), _1 = cqq._1 })
  end
  local crv = { _1 = 1, _2 = nil }
  writeln(_dollarshow(function(x)
    return tostring(x)
  end, _dollartraverse(nil, _dollardApplicativeajd({
    ["×"] = function(x) return function(ys) return _dollar_d7(nil, x, ys) end end,
    zero = Nil
  }), function(ckj) return Cons({ _1 = ckj._1, _2 = Nil }) end, Cons({
    _1 = crv,
    _2 = Cons({ _1 = crv, _2 = Cons({ _1 = crv, _2 = Nil }) })
  }))))
end
