do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local bottom = nil
  local ci = (function ()
    local cg = bottom
    local a = cg(1)
    local b = cg(2)
    local c = cg(3)
    local ch = {
      _1 = b,
      _2 = {
        _1 = c,
        _2 = a
      }
    }
    local ce = bottom
    return ce(ch)
  end)()
end
