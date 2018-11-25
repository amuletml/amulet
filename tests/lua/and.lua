do
  local function main(f) return f(1) and f(2) end
  local bottom = nil
  (nil)(main)
end
