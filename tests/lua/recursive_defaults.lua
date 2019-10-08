do
  local use = print
  local function _dollardShowdd(tmp)
    return {
      show = function(tmp0) return "()" end,
      ["show'"] = function(ez) return _dollardShowdd(nil).show(ez) end
    }
  end
  use(_dollardShowdd(nil).show)
end
