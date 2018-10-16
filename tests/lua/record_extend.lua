do
  local bottom = nil
  local ay = {}
  for k, v in pairs(bottom) do
    ay[k] = v
  end
  ay.x = 1
  bottom(ay)
end
