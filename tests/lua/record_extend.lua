do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local bottom = nil
  local au = (function ()
    local __o, __n = bottom, {
      
    }
    for k, v in pairs(__o) do
      __n[k] = v
    end
    __n.x = 1
    local at = __n
    local aq = bottom
    return aq(at)
  end)()
end
