do
  local bottom = nil
  local aw = {}
  for k, v in pairs(bottom) do
    aw[k] = v
  end
  aw.x = 1
  bottom(aw)
end
