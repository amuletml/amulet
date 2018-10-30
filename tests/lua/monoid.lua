do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeafh(aha)
    return {
      ["<*>"] = function(ccf) return function(ccc) return aha["×"](ccf)(ccc) end end,
      pure = function(ccm) return aha.zero end,
      ["Applicative$kp"] = function(cbr) return function(cbo) return cbo end end
    }
  end
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollarlShowate(auq)
    return function(cw)
      if cw.__tag == "Cons" then
        local cej = cw[1]
        return auq(cej._1) .. " :: " .. _dollarlShowate(auq)(cej._2)
      elseif cw.__tag == "Nil" then
        return "Nil"
      end
    end
  end
  local function _dollarlTraversablebit(cid)
    return function(bkn)
      local cal = bkn["Applicative$kp"]
      return function(k)
        return function(cy)
          if cy.__tag == "Cons" then
            local chy = cy[1]
            return bkn["<*>"](cal(_colon_colon)(k(chy._1)))(_dollarlTraversablebit(__builtin_unit)(bkn)(k)(chy._2))
          elseif cy.__tag == "Nil" then
            return bkn.pure(Nil)
          end
        end
      end
    end
  end
  local function _dollarlMonoidbne(cit)
    return {
      zero = Nil,
      ["×"] = function(x)
        return function(ys)
          if x.__tag == "Cons" then
            local cin = x[1]
            return Cons({ _1 = cin._1, _2 = _dollarlMonoidbne(__builtin_unit)["×"](cin._2)(ys) })
          elseif x.__tag == "Nil" then
            return ys
          end
        end
      end
    }
  end
  local cle = _dollarlTraversablebit(__builtin_unit)(_dollardApplicativeafh(_dollarlMonoidbne(__builtin_unit)))
  local cmf = _dollardApplicativeafh(_dollarlMonoidbne(__builtin_unit))
  local cal = cmf["Applicative$kp"]
  local cjv = { _1 = 1, _2 = __builtin_unit }
  writeln(_dollarlShowate(function(x) return tostring(x) end)(cle(function(cdd)
    local b = cdd._2
    return cal(function(cu) return { _1 = cu, _2 = b } end)(Cons({ _1 = cdd._1, _2 = Nil }))
  end)(Cons({ _1 = cjv, _2 = Cons({ _1 = cjv, _2 = Cons({ _1 = cjv, _2 = Nil }) }) }))))
end
