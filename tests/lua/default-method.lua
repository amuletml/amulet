do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local function _dollardShowcj(fq)
    return {
      show = function(fm) return "()" end,
      show_tail = function(eh) return "tail" .. _dollardShowcj(__builtin_unit).show(eh) end
    }
  end
  local fw = _dollardShowcj(__builtin_unit).show_tail(__builtin_unit)
  if fw == "" then

  else
    error("Pattern matching failure in let expression at default-method.ml[12:5 ..12:6]")
  end
end
