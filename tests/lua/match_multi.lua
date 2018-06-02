do
  local __builtin_unit = {
    ["__tag"] = "__builtin_unit"
  }
  local function main (f)
    local am = f(__builtin_unit)
    return am.a + am.b
  end
  main()
end
