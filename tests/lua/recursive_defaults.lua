do
  local use = print
  local function _dollardShowea(tmp)
    return {
      show = function(tmp0) return "()" end,
      ["show'"] = function(gh) return _dollardShowea(nil).show(gh) end
    }
  end
  use(_dollardShowea(nil).show)
end
