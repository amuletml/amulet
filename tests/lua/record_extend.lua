do
  local bottom = nil
  local at = {}
  for k, v in pairs(bottom) do
    at[k] = v
  end
  at.x = 1
  bottom(at)
end
