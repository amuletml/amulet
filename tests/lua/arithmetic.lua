do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local bottom = nil
  local cg = (function ()
    local ce = bottom
    local cf = ce(1) + ce(2)
    local cd = bottom
    return cd(cf)
  end)()
end
