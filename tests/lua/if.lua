do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local bottom = nil
  local bd = (function ()
    local bb = bottom
    if bb(1) then
      local bk = bb(2)
      local ba = bottom
      return ba(bk)
    else
      local bl = bb(3)
      local ba = bottom
      return ba(bl)
    end
  end)()
end
