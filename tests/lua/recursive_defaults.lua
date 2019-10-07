do
  local use = print
  local function _dollardShowda(tmp)
    return {
      show = function(tmp0) return "()" end,
      ["show'"] = function(eu) return _dollardShowda(nil).show(eu) end
    }
  end
  use(_dollardShowda(nil).show)
end
