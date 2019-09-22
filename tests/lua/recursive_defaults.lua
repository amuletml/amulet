do
  local use = print
  local function _dollardShowcy(gc)
    return {
      show = function(fu) return "()" end,
      ["show'"] = function(er) return _dollardShowcy(nil).show(er) end
    }
  end
  use(_dollardShowcy(nil).show)
end
