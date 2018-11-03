do
  local bottom = nil
  local function main(bt) return (bt.a + bt.b) * bt.c end
  bottom(main)
end
