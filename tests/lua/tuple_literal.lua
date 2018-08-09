do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local bottom = nil
  local ck = (function ()
    local ci = bottom
    local a = ci(1)
    local b = ci(2)
    local c = ci(3)
    local cj = {
      _1 = b,
      _2 = {
        _1 = c,
        _2 = a
      }
    }
    local cg = bottom
    return cg(cj)
  end)()
end
