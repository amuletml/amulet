do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local function _dollarlShowcj(fm)
    return {
      show = function(fi) return "()" end,
      show_tail = function(x) return "tail" .. _dollarlShowcj(__builtin_unit).show(x) end
    }
  end
  local fs = _dollarlShowcj(__builtin_unit).show_tail(__builtin_unit)
  if fs == "" then

  else
    error("Pattern matching failure in let expression at default-method.ml[12:5 ..12:6]")
  end
end
