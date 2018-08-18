do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local bottom = nil
  local ce = (function ()
    local cc = bottom
    local cd = cc(1) + cc(2)
    local cb = bottom
    return cb(cd)
  end)()
end
