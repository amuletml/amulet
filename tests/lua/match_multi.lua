do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local function main (f)
    local ai = f(__builtin_unit)
    return ai.a + ai.b
  end
  main()
end
