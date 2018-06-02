do
  local __builtin_unit = {
    ["__tag"] = "__builtin_unit"
  }
  local function main (f)
    local al = f(__builtin_unit)
    return al.a + al.b
  end
  main()
end
