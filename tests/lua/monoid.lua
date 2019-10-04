do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeajd(cks)
    return {
      ["<*>"] = function(cjs) return function(cjp) return cks["×"](cjs)(cjp) end end,
      pure = function(ckd) return cks.zero end,
      ["Applicative$ma"] = function(ciw) return function(cit) return cit end end
    }
  end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollarshow(aym, cs)
    if cs.__tag == "Nil" then
      return "Nil"
    end
    local cmv = cs[1]
    return aym(cmv._1) .. " :: " .. _dollarshow(aym, cmv._2)
  end
  local function _dollartraverse(bpb, csi, k, cu)
    if cu.__tag == "Nil" then
      return csi.pure(Nil)
    end
    local csf = cu[1]
    return csi["<*>"](csi["Applicative$ma"](_colon_colon)(k(csf._1)))(_dollartraverse(nil, csi, k, csf._2))
  end
  local function _dollar_d7(bua, x, ys)
    if x.__tag == "Nil" then
      return ys
    end
    local ctn = x[1]
    return Cons({ _1 = ctn._1, _2 = _dollar_d7(nil, ctn._2, ys) })
  end
  local cux = { _1 = 1, _2 = nil }
  writeln(_dollarshow(function(x)
    return tostring(x)
  end, _dollartraverse(nil, _dollardApplicativeajd({
    zero = Nil,
    ["×"] = function(x) return function(ys) return _dollar_d7(nil, x, ys) end end
  }), function(cle) return Cons({ _1 = cle._1, _2 = Nil }) end, Cons({
    _1 = cux,
    _2 = Cons({ _1 = cux, _2 = Cons({ _1 = cux, _2 = Nil }) })
  }))))
end
