do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeafa(agr)
    return {
      pure = function(cbs) return agr.zero end,
      ["<*>"] = function(cbl) return function(cbi) return agr["×"](cbl)(cbi) end end,
      ["Applicative$ko"] = { ["<$>"] = function(caw) return function(cat) return cat end end }
    }
  end
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollarlShowasq(aua)
    return {
      show = function(cw)
        if cw.__tag == "Cons" then
          local cdp = cw[1]
          return aua.show(cdp._1) .. " :: " .. _dollarlShowasq(aua).show(cdp._2)
        elseif cw.__tag == "Nil" then
          return "Nil"
        end
      end
    }
  end
  local function _dollarlTraversablebib(chq)
    return {
      traverse = function(bju)
        local bym = bju["Applicative$ko"]
        return function(k)
          return function(cy)
            if cy.__tag == "Cons" then
              local chk = cy[1]
              return bju["<*>"](bym["<$>"](_colon_colon)(k(chk._1)))(_dollarlTraversablebib(__builtin_unit).traverse(bju)(k)(chk._2))
            elseif cy.__tag == "Nil" then
              return bju.pure(Nil)
            end
          end
        end
      end
    }
  end
  local function _dollarlMonoidbmk(cig)
    return {
      ["×"] = function(x)
        return function(ys)
          if x.__tag == "Cons" then
            local cia = x[1]
            return Cons({ _1 = cia._1, _2 = _dollarlMonoidbmk(__builtin_unit)["×"](cia._2)(ys) })
          elseif x.__tag == "Nil" then
            return ys
          end
        end
      end,
      zero = Nil
    }
  end
  local clp = _dollardApplicativeafa(_dollarlMonoidbmk(__builtin_unit))
  local bym = clp["Applicative$ko"]
  local cjq = { _1 = 1, _2 = __builtin_unit }
  local bxd = _dollardApplicativeafa(_dollarlMonoidbmk(__builtin_unit))
  writeln(_dollarlShowasq({
    show = function(x) return tostring(x) end
  }).show(_dollarlTraversablebib(__builtin_unit).traverse(bxd)(function(ccj)
    local b = ccj._2
    return bym["<$>"](function(cu) return { _1 = cu, _2 = b } end)(Cons({ _1 = ccj._1, _2 = Nil }))
  end)(Cons({ _1 = cjq, _2 = Cons({ _1 = cjq, _2 = Cons({ _1 = cjq, _2 = Nil }) }) }))))
end
