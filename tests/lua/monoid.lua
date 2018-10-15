do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeafa(agr)
    return {
      pure = function(cca) return agr.zero end,
      ["<*>"] = function(cbt) return function(cbq) return agr["×"](cbt)(cbq) end end,
      ["Applicative$ko"] = { ["<$>"] = function(cbe) return function(cbb) return cbb end end }
    }
  end
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollarlShowasr(aub)
    return {
      show = function(cw)
        if cw.__tag == "Cons" then
          local cdx = cw[1]
          return aub.show(cdx._1) .. " :: " .. _dollarlShowasr(aub).show(cdx._2)
        elseif cw.__tag == "Nil" then
          return "Nil"
        end
      end
    }
  end
  local function _dollarlTraversablebic(chy)
    return {
      traverse = function(bjv)
        local byu = bjv["Applicative$ko"]
        return function(k)
          return function(cy)
            if cy.__tag == "Cons" then
              local chs = cy[1]
              return bjv["<*>"](byu["<$>"](_colon_colon)(k(chs._1)))(_dollarlTraversablebic(__builtin_unit).traverse(bjv)(k)(chs._2))
            elseif cy.__tag == "Nil" then
              return bjv.pure(Nil)
            end
          end
        end
      end
    }
  end
  local function _dollarlMonoidbmm(cio)
    return {
      ["×"] = function(x)
        return function(ys)
          if x.__tag == "Cons" then
            local cii = x[1]
            return Cons({ _1 = cii._1, _2 = _dollarlMonoidbmm(__builtin_unit)["×"](cii._2)(ys) })
          elseif x.__tag == "Nil" then
            return ys
          end
        end
      end,
      zero = Nil
    }
  end
  local clx = _dollardApplicativeafa(_dollarlMonoidbmm(__builtin_unit))
  local byu = clx["Applicative$ko"]
  local cjy = { _1 = 1, _2 = __builtin_unit }
  local bxf = _dollardApplicativeafa(_dollarlMonoidbmm(__builtin_unit))
  writeln(_dollarlShowasr({
    show = function(x) return tostring(x) end
  }).show(_dollarlTraversablebic(__builtin_unit).traverse(bxf)(function(ccr)
    local b = ccr._2
    return byu["<$>"](function(cu) return { _1 = cu, _2 = b } end)(Cons({ _1 = ccr._1, _2 = Nil }))
  end)(Cons({ _1 = cjy, _2 = Cons({ _1 = cjy, _2 = Cons({ _1 = cjy, _2 = Nil }) }) }))))
end
