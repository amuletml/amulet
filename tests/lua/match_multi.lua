do
  local __builtin_unit = {
    ["__tag"] = "__builtin_unit"
  }
  local function main (f)
    local ah = f(__builtin_unit)
    return ah.a + ah.b
  end
  main()
end
