do
  local __builtin_unit = {
    ["__tag"] = "__builtin_unit"
  }
  local function main (f)
    local a = f(1.0)
    local b = f(2.0)
    return a + b
  end
  main()
end
