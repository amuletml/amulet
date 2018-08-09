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
  local ds = (function ()
    local _do = bottom
    local a = _do(1)
    local dq = bottom
    if dq.__tag == "None" then
      local fx = _do(a)
      local dn = bottom
      return dn(fx)
    elseif dq.__tag == "Some" then
      local fy = _do(a + dq[1] * 2)
      local dn = bottom
      return dn(fy)
    end
  end)()
end
