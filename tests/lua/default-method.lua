do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local function _dollardShowci(fl)
    return {
      show = function(fh) return "()" end,
      show_tail = function(x) return "tail" .. _dollardShowci(__builtin_unit).show(x) end
    }
  end
  local fr = _dollardShowci(__builtin_unit).show_tail(__builtin_unit)
  if fr == "" then

  else
    error("Pattern matching failure in let expression at default-method.ml[12:5 ..12:6]")
  end
end
