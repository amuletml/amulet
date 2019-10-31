do
  local Nil = { __tag = "Nil" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeaou(tmp)
    return {
      pure = function(tmp0) return tmp.zero end,
      ["<*>"] = function(tmp0) return function(tmp1) return tmp["×"](tmp0)(tmp1) end end,
      ["Applicative$ky"] = function(tmp0) return function(tmp1) return tmp1 end end
    }
  end
  local function _colon_colon(x)
    return function(y) return { { _1 = x, _2 = y }, __tag = "Cons" } end
  end
  local function _dollarshow(bgb, x)
    if x.__tag == "Nil" then return "Nil" end
    local tmp = x[1]
    return bgb(tmp._1) .. " :: " .. _dollarshow(bgb, tmp._2)
  end
  local function _dollartraverse(cak, tmp, k, x)
    if x.__tag == "Nil" then return tmp.pure(Nil) end
    local tmp0 = x[1]
    return tmp["<*>"](tmp["Applicative$ky"](_colon_colon)(k(tmp0._1)))(_dollartraverse(nil, tmp, k, tmp0._2))
  end
  local function _dollar_d7(cgs, x, ys)
    if x.__tag == "Nil" then return ys end
    local tmp = x[1]
    return { { _1 = tmp._1, _2 = _dollar_d7(nil, tmp._2, ys) }, __tag = "Cons" }
  end
  local tmp = { _1 = 1, _2 = nil }
  writeln(_dollarshow(function(x)
    return tostring(x)
  end, _dollartraverse(nil, _dollardApplicativeaou({
    ["×"] = function(x) return function(ys) return _dollar_d7(nil, x, ys) end end,
    zero = Nil
  }), function(tmp0) return { { _1 = tmp0._1, _2 = Nil }, __tag = "Cons" } end, {
    {
      _1 = tmp,
      _2 = { { _1 = tmp, _2 = { { _1 = tmp, _2 = Nil }, __tag = "Cons" } }, __tag = "Cons" }
    },
    __tag = "Cons"
  })))
end
