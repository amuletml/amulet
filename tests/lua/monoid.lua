do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeaew(caw)
    return {
      pure = function(cai) return caw.zero end,
      ["<*>"] = function(caz) return function(caw0) return caw["×"](caz)(caw0) end end,
      ["Applicative$kl"] = function(cah) return function(cae) return cae end end
    }
  end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollarshow(ass, cs)
    if cs.__tag == "Nil" then
      return "Nil"
    elseif cs.__tag == "Cons" then
      local ccr = cs[1]
      return ass(ccr._1) .. " :: " .. _dollarshow(ass, ccr._2)
    end
  end
  local function _dollartraverse(bhn, cgr, k, cu)
    if cu.__tag == "Nil" then
      return cgr.pure(Nil)
    elseif cu.__tag == "Cons" then
      local cgo = cu[1]
      return cgr["<*>"](cgr["Applicative$kl"](_colon_colon)(k(cgo._1)))(_dollartraverse(nil, cgr, k, cgo._2))
    end
  end
  local function _dollar_d7(blz, x, ys)
    if x.__tag == "Nil" then
      return ys
    elseif x.__tag == "Cons" then
      local cho = x[1]
      return Cons({ _2 = _dollar_d7(nil, cho._2, ys), _1 = cho._1 })
    end
  end
  local cit = { _1 = 1, _2 = nil }
  writeln(_dollarshow(function(x)
    return tostring(x)
  end, _dollartraverse(nil, _dollardApplicativeaew({
    zero = Nil,
    ["×"] = function(x) return function(ys) return _dollar_d7(nil, x, ys) end end
  }), function(cbh) return Cons({ _1 = cbh._1, _2 = Nil }) end, Cons({
    _1 = cit,
    _2 = Cons({ _1 = cit, _2 = Cons({ _1 = cit, _2 = Nil }) })
  }))))
end
