do
  local bottom = nil
  local function main(bn) return (bn.a + bn.b) * bn.c end
  bottom(main)
end
