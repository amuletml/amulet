do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeais(chu)
    return {
      pure = function(chg) return chu.zero end,
      ["<*>"] = function(cgx) return function(cgu) return chu["×"](cgx)(cgu) end end,
      ["Applicative$ma"] = function(cgf) return function(cgc) return cgc end end
    }
  end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollarshow(axo, cs)
    if cs.__tag == "Nil" then
      return "Nil"
    elseif cs.__tag == "Cons" then
      local cjp = cs[1]
      return axo(cjp._1) .. " :: " .. _dollarshow(axo, cjp._2)
    end
  end
  local function _dollartraverse(bnq, cnp, k, cu)
    if cu.__tag == "Nil" then
      return cnp.pure(Nil)
    elseif cu.__tag == "Cons" then
      local cnm = cu[1]
      return cnp["<*>"](cnp["Applicative$ma"](_colon_colon)(k(cnm._1)))(_dollartraverse(nil, cnp, k, cnm._2))
    end
  end
  local function _dollar_d7(bsg, x, ys)
    if x.__tag == "Nil" then
      return ys
    elseif x.__tag == "Cons" then
      local com = x[1]
      return Cons({ _1 = com._1, _2 = _dollar_d7(nil, com._2, ys) })
    end
  end
  local cpr = { _1 = 1, _2 = nil }
  writeln(_dollarshow(function(x)
    return tostring(x)
  end, _dollartraverse(nil, _dollardApplicativeais({
    zero = Nil,
    ["×"] = function(x) return function(ys) return _dollar_d7(nil, x, ys) end end
  }), function(cif) return Cons({ _1 = cif._1, _2 = Nil }) end, Cons({
    _1 = cpr,
    _2 = Cons({ _1 = cpr, _2 = Cons({ _1 = cpr, _2 = Nil }) })
  }))))
end
