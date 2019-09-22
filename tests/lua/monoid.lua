do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeaja(cis)
    return {
      pure = function(cie) return cis.zero end,
      ["<*>"] = function(chv) return function(chs) return cis["×"](chv)(chs) end end,
      ["Applicative$ma"] = function(chd) return function(cha) return cha end end
    }
  end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollarshow(ayb, cs)
    if cs.__tag == "Nil" then
      return "Nil"
    end
    local ckn = cs[1]
    return ayb(ckn._1) .. " :: " .. _dollarshow(ayb, ckn._2)
  end
  local function _dollartraverse(boj, con, k, cu)
    if cu.__tag == "Nil" then
      return con.pure(Nil)
    end
    local cok = cu[1]
    return con["<*>"](con["Applicative$ma"](_colon_colon)(k(cok._1)))(_dollartraverse(nil, con, k, cok._2))
  end
  local function _dollar_d7(btc, x, ys)
    if x.__tag == "Nil" then
      return ys
    end
    local cpk = x[1]
    return Cons({ _2 = _dollar_d7(nil, cpk._2, ys), _1 = cpk._1 })
  end
  local cqp = { _1 = 1, _2 = nil }
  writeln(_dollarshow(function(x)
    return tostring(x)
  end, _dollartraverse(nil, _dollardApplicativeaja({
    ["×"] = function(x) return function(ys) return _dollar_d7(nil, x, ys) end end,
    zero = Nil
  }), function(cjd) return Cons({ _1 = cjd._1, _2 = Nil }) end, Cons({
    _1 = cqp,
    _2 = Cons({ _1 = cqp, _2 = Cons({ _1 = cqp, _2 = Nil }) })
  }))))
end
