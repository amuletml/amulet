do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local function _dollardShowcj(fo)
    return {
      show = function(fk) return "()" end,
      show_tail = function(x) return "tail" .. _dollardShowcj(__builtin_unit).show(x) end
    }
  end
  local fu = _dollardShowcj(__builtin_unit).show_tail(__builtin_unit)
  if fu == "" then

  else
    error("Pattern matching failure in let expression at default-method.ml[12:5 ..12:6]")
  end
end
