do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  local function main(bn) return (bn.a + bn.b) * bn.c end
  bottom(main)
end
