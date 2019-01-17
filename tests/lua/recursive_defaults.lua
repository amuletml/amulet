do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local use = print
  local function _dollardShowci(fl)
    return {
      show = function(fh) return "()" end,
      ["show'"] = function(dy) return _dollardShowci(__builtin_unit).show(dy) end
    }
  end
  use(_dollardShowci(__builtin_unit).show)
end
