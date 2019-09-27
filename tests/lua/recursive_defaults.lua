do
  local use = print
  local function _dollardShowcz(gf)
    return {
      show = function(fx) return "()" end,
      ["show'"] = function(et) return _dollardShowcz(nil).show(et) end
    }
  end
  use(_dollardShowcz(nil).show)
end
