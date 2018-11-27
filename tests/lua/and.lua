do
  local function main(f) return f(1) and f(2) end
  (nil)(main)
end
