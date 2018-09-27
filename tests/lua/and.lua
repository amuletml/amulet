do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local function main(f) return f(1) and f(2) end
  local bottom = nil
  bottom(main)
end
