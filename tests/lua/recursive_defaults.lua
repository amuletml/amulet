do
  local use = print
  local function _dollardShowcx(gb)
    return {
      show = function(ft) return "()" end,
      ["show'"] = function(eq) return _dollardShowcx(nil).show(eq) end
    }
  end
  use(_dollardShowcx(nil).show)
end
