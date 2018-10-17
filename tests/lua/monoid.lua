do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local tostring = tostring
  local writeln = print
  local function _dollardApplicativeaff(agy)
    return {
      pure = function(ccn) return agy.zero end,
      ["<*>"] = function(ccg) return function(ccd) return agy["×"](ccg)(ccd) end end,
      ["Applicative$kp"] = { ["<$>"] = function(cbr) return function(cbo) return cbo end end }
    }
  end
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function _colon_colon(x) return function(y) return Cons({ _1 = x, _2 = y }) end end
  local function _dollarlShowasy(auk)
    return {
      show = function(cw)
        if cw.__tag == "Cons" then
          local cek = cw[1]
          return auk.show(cek._1) .. " :: " .. _dollarlShowasy(auk).show(cek._2)
        elseif cw.__tag == "Nil" then
          return "Nil"
        end
      end
    }
  end
  local function _dollarlTraversablebin(cil)
    return {
      traverse = function(bkh)
        local cah = bkh["Applicative$kp"]
        return function(k)
          return function(cy)
            if cy.__tag == "Cons" then
              local cif = cy[1]
              return bkh["<*>"](cah["<$>"](_colon_colon)(k(cif._1)))(_dollarlTraversablebin(__builtin_unit).traverse(bkh)(k)(cif._2))
            elseif cy.__tag == "Nil" then
              return bkh.pure(Nil)
            end
          end
        end
      end
    }
  end
  local function _dollarlMonoidbmy(cjb)
    return {
      ["×"] = function(x)
        return function(ys)
          if x.__tag == "Cons" then
            local civ = x[1]
            return Cons({ _1 = civ._1, _2 = _dollarlMonoidbmy(__builtin_unit)["×"](civ._2)(ys) })
          elseif x.__tag == "Nil" then
            return ys
          end
        end
      end,
      zero = Nil
    }
  end
  local cmk = _dollardApplicativeaff(_dollarlMonoidbmy(__builtin_unit))
  local cah = cmk["Applicative$kp"]
  local ckl = { _1 = 1, _2 = __builtin_unit }
  local bxs = _dollardApplicativeaff(_dollarlMonoidbmy(__builtin_unit))
  writeln(_dollarlShowasy({
    show = function(x) return tostring(x) end
  }).show(_dollarlTraversablebin(__builtin_unit).traverse(bxs)(function(cde)
    local b = cde._2
    return cah["<$>"](function(cu) return { _1 = cu, _2 = b } end)(Cons({ _1 = cde._1, _2 = Nil }))
  end)(Cons({ _1 = ckl, _2 = Cons({ _1 = ckl, _2 = Cons({ _1 = ckl, _2 = Nil }) }) }))))
end
