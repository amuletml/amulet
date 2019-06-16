do
  local use = print
  local function _dollardShowci(fj)
    return {
      show = function(fb) return "()" end,
      ["show'"] = function(dz) return _dollardShowci(nil).show(dz) end
    }
  end
  use(_dollardShowci(nil).show)
end
