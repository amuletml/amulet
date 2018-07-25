do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local function main (f)
    return f(1) + f(2)
  end
  main()
end
