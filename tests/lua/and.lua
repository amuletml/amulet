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
  local cy = (function ()
    local cw = bottom
    if cw(1) then
      local cu = __builtin_Lazy(function (cq)
        return cw(2)
      end)
      local cm = __builtin_force
      local dt = cm(cu)
      local cv = bottom
      return cv(dt)
    else
      return bottom(false)
    end
  end)()
end
