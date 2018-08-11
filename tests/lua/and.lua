do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local function __builtin_force (x)
    if x[2.0] then
      return x[1.0]
    else
      x[1.0], x[2.0] = x[1.0](__builtin_unit), true
      return x[1.0]
    end
  end
  local function __builtin_Lazy (x)
    return {
      [1.0] = x,
      [2.0] = false,
      __tag = "lazy"
    }
  end
  local bottom = nil
  local cx = (function ()
    local cv = bottom
    if cv(1) then
      local ct = __builtin_Lazy(function (cq)
        return cv(2)
      end)
      local cm = __builtin_force
      local dq = cm(ct)
      local cu = bottom
      return cu(dq)
    else
      return bottom(false)
    end
  end)()
end
