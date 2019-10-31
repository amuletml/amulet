do
  local use = print
  local function _dollardShowdu(tmp)
    return {
      show = function(tmp0) return "()" end,
      ["show'"] = function(fy) return _dollardShowdu(nil).show(fy) end
    }
  end
  use(_dollardShowdu(nil).show)
end
