do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeafd(agv)
    return {
      ["<*>"] = function(ccc) return function(cbz) return agv["×"](ccc)(cbz) end end,
      pure = function(ccj) return agv.zero end,
      ["Applicative$kp"] = _dollardApplicativeafd(agv)["Applicative$kp"]
    }
  end
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollardShowatb(aun)
    return function(cw)
      if cw.__tag == "Cons" then
        local cej = cw[1]
        return aun(cej._1) .. " :: " .. _dollardShowatb(aun)(cej._2)
      elseif cw.__tag == "Nil" then
        return "Nil"
      end
    end
  end
  local function _dollardTraversablebip(cig)
    return function(cid)
      return function(k)
        return function(cy)
          if cy.__tag == "Cons" then
            local chz = cy[1]
            return cid["<*>"](cid["Applicative$kp"](_colon_colon)(k(chz._1)))(_dollardTraversablebip(__builtin_unit)(cid)(k)(chz._2))
          elseif cy.__tag == "Nil" then
            return cid.pure(Nil)
          end
        end
      end
    end
  end
  local function _dollardMonoidbmz(cix)
    return {
      zero = Nil,
      ["×"] = function(x)
        return function(ys)
          if x.__tag == "Cons" then
            local cir = x[1]
            return Cons({ _1 = cir._1, _2 = _dollardMonoidbmz(__builtin_unit)["×"](cir._2)(ys) })
          elseif x.__tag == "Nil" then
            return ys
          end
        end
      end
    }
  end
  local cjz = { _1 = 1, _2 = __builtin_unit }
  local cli = _dollardTraversablebip(__builtin_unit)(_dollardApplicativeafd(_dollardMonoidbmz(__builtin_unit)))
  local cmj = _dollardApplicativeafd(_dollardMonoidbmz(__builtin_unit))
  local cai = cmj["Applicative$kp"]
  writeln(_dollardShowatb(function(x) return tostring(x) end)(cli(function(cdc)
    local b = cdc._2
    return cai(function(cu) return { _1 = cu, _2 = b } end)(Cons({ _1 = cdc._1, _2 = Nil }))
  end)(Cons({ _1 = cjz, _2 = Cons({ _1 = cjz, _2 = Cons({ _1 = cjz, _2 = Nil }) }) }))))
end
