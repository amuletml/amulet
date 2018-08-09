do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local bottom = nil
  local bm = (function ()
    local bc = bottom(__builtin_unit)
    local bl = bc.a + bc.b
    local bi = bottom
    return bi(bl)
  end)()
end
