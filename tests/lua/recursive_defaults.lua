do
  local use = print
  local function _dollardShowcz(tmp)
    return {
      show = function(tmp0) return "()" end,
      ["show'"] = function(et) return _dollardShowcz(nil).show(et) end
    }
  end
  use(_dollardShowcz(nil).show)
end
