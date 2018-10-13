do
  local bottom = nil
  local function main(bs) return (bs.a + bs.b) * bs.c end
  bottom(main)
end
