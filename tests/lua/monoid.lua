do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeait(cia)
    return {
      pure = function(chm) return cia.zero end,
      ["<*>"] = function(chd) return function(cha) return cia["×"](chd)(cha) end end,
      ["Applicative$ma"] = function(cgl) return function(cgi) return cgi end end
    }
  end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollarshow(axr, cs)
    if cs.__tag == "Nil" then
      return "Nil"
    elseif cs.__tag == "Cons" then
      local cjv = cs[1]
      return axr(cjv._1) .. " :: " .. _dollarshow(axr, cjv._2)
    end
  end
  local function _dollartraverse(bnu, cnv, k, cu)
    if cu.__tag == "Nil" then
      return cnv.pure(Nil)
    elseif cu.__tag == "Cons" then
      local cns = cu[1]
      return cnv["<*>"](cnv["Applicative$ma"](_colon_colon)(k(cns._1)))(_dollartraverse(nil, cnv, k, cns._2))
    end
  end
  local function _dollar_d7(bsk, x, ys)
    if x.__tag == "Nil" then
      return ys
    elseif x.__tag == "Cons" then
      local cos = x[1]
      return Cons({ _2 = _dollar_d7(nil, cos._2, ys), _1 = cos._1 })
    end
  end
  local cpx = { _1 = 1, _2 = nil }
  writeln(_dollarshow(function(x)
    return tostring(x)
  end, _dollartraverse(nil, _dollardApplicativeait({
    ["×"] = function(x) return function(ys) return _dollar_d7(nil, x, ys) end end,
    zero = Nil
  }), function(cil) return Cons({ _2 = Nil, _1 = cil._1 }) end, Cons({
    _1 = cpx,
    _2 = Cons({ _1 = cpx, _2 = Cons({ _1 = cpx, _2 = Nil }) })
  }))))
end
