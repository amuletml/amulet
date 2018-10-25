do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeaff(agy)
    return {
      pure = function(ccg) return agy.zero end,
      ["<*>"] = function(cbz) return function(cbw) return agy["×"](cbz)(cbw) end end,
      ["Applicative$kp"] = function(cbl) return function(cbi) return cbi end end
    }
  end
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollarlShowasy(auk)
    return function(cw)
      if cw.__tag == "Cons" then
        local ced = cw[1]
        return auk(ced._1) .. " :: " .. _dollarlShowasy(auk)(ced._2)
      elseif cw.__tag == "Nil" then
        return "Nil"
      end
    end
  end
  local function _dollarlTraversablebin(chx)
    return function(bkh)
      local caf = bkh["Applicative$kp"]
      return function(k)
        return function(cy)
          if cy.__tag == "Cons" then
            local chs = cy[1]
            return bkh["<*>"](caf(_colon_colon)(k(chs._1)))(_dollarlTraversablebin(__builtin_unit)(bkh)(k)(chs._2))
          elseif cy.__tag == "Nil" then
            return bkh.pure(Nil)
          end
        end
      end
    end
  end
  local function _dollarlMonoidbmy(cin)
    return {
      ["×"] = function(x)
        return function(ys)
          if x.__tag == "Cons" then
            local cih = x[1]
            return Cons({ _1 = cih._1, _2 = _dollarlMonoidbmy(__builtin_unit)["×"](cih._2)(ys) })
          elseif x.__tag == "Nil" then
            return ys
          end
        end
      end,
      zero = Nil
    }
  end
  local cjx = { _1 = 1, _2 = __builtin_unit }
  local bxs = _dollardApplicativeaff(_dollarlMonoidbmy(__builtin_unit))
  local clw = _dollardApplicativeaff(_dollarlMonoidbmy(__builtin_unit))
  local caf = clw["Applicative$kp"]
  writeln(_dollarlShowasy(function(x)
    return tostring(x)
  end)(_dollarlTraversablebin(__builtin_unit)(bxs)(function(ccx)
    local b = ccx._2
    return caf(function(cu) return { _1 = cu, _2 = b } end)(Cons({ _1 = ccx._1, _2 = Nil }))
  end)(Cons({ _1 = cjx, _2 = Cons({ _1 = cjx, _2 = Cons({ _1 = cjx, _2 = Nil }) }) }))))
end
