do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local None = {
    __tag = "None"
  }
  local function Some (x)
    return {
      __tag = "Some",
      [1] = x
    }
  end
  local bottom = nil
  local dq = (function ()
    local dm = bottom
    local a = dm(1)
    local _do = bottom
    if _do.__tag == "None" then
      local fz = dm(a)
      local dl = bottom
      return dl(fz)
    elseif _do.__tag == "Some" then
      local ga = dm(a + _do[1] * 2)
      local dl = bottom
      return dl(ga)
    end
  end)()
end
