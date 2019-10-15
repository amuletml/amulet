do
  local use = print
  local function _dollardShowdd(tmp)
    return {
      show = function(tmp0) return "()" end,
      ["show'"] = function(fa) return _dollardShowdd(nil).show(fa) end
    }
  end
  use(_dollardShowdd(nil).show)
end
