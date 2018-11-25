do
  local bottom = nil
  local function main(bt) return (bt.a + bt.b) * bt.c end
  (nil)(main)
end
