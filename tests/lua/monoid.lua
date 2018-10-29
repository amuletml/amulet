do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeafh(aha)
    return {
      ["<*>"] = function(cbz) return function(cbw) return aha["×"](cbz)(cbw) end end,
      pure = function(ccg) return aha.zero end,
      ["Applicative$kp"] = function(cbl) return function(cbi) return cbi end end
    }
  end
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollarlShowate(auq)
    return function(cw)
      if cw.__tag == "Cons" then
        local ced = cw[1]
        return auq(ced._1) .. " :: " .. _dollarlShowate(auq)(ced._2)
      elseif cw.__tag == "Nil" then
        return "Nil"
      end
    end
  end
  local function _dollarlTraversablebit(chx)
    return function(bkn)
      local caf = bkn["Applicative$kp"]
      return function(k)
        return function(cy)
          if cy.__tag == "Cons" then
            local chs = cy[1]
            return bkn["<*>"](caf(_colon_colon)(k(chs._1)))(_dollarlTraversablebit(__builtin_unit)(bkn)(k)(chs._2))
          elseif cy.__tag == "Nil" then
            return bkn.pure(Nil)
          end
        end
      end
    end
  end
  local function _dollarlMonoidbne(cin)
    return {
      zero = Nil,
      ["×"] = function(x)
        return function(ys)
          if x.__tag == "Cons" then
            local cih = x[1]
            return Cons({ _1 = cih._1, _2 = _dollarlMonoidbne(__builtin_unit)["×"](cih._2)(ys) })
          elseif x.__tag == "Nil" then
            return ys
          end
        end
      end
    }
  end
  local cjp = { _1 = 1, _2 = __builtin_unit }
  writeln(_dollarlShowate(function(x)
    return tostring(x)
  end)(_dollarlTraversablebit(__builtin_unit)(_dollardApplicativeafh(_dollarlMonoidbne(__builtin_unit)))(function(ccx)
    return Cons({ _1 = ccx._1, _2 = Nil })
  end)(Cons({ _1 = cjp, _2 = Cons({ _1 = cjp, _2 = Cons({ _1 = cjp, _2 = Nil }) }) }))))
end
