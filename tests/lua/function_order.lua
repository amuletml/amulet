do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local bottom = nil
  local db = (function ()
    local cy = bottom
    local a = cy(1)
    local b = cy(2)
    local c = cy(3)
    local da = bottom
    return da(b)(c)(a)
  end)()
end
