do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local bottom = nil
  local bd = (function ()
    local bb = bottom
    if bb(1) then
      local bh = bb(2)
      local ba = bottom
      return ba(bh)
    else
      local bi = bb(3)
      local ba = bottom
      return ba(bi)
    end
  end)()
end
