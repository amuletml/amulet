do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local bottom = nil
  local dd = (function ()
    local da = bottom
    local a = da(1)
    local b = da(2)
    local c = da(3)
    local dc = bottom
    return dc(b)(c)(a)
  end)()
end
