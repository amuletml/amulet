do
  local __builtin_unit = {
    ["__tag"] = "__builtin_unit"
  }
  local function main (f)
    local ak = f(__builtin_unit)
    return ak.a + ak.b
  end
  main()
end
