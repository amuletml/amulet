do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local bottom = nil
  if bottom(1) then
    bottom(bottom(2))
  else
    bottom(bottom(3))
  end
end
