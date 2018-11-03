do
  local bottom = nil
  local ba = {}
  for k, v in pairs(bottom) do
    ba[k] = v
  end
  ba.x = 1
  bottom(ba)
end
