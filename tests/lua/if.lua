do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local function main (f)
    if f(1) then
      return f(2)
    else
      return f(3)
    end
  end
  main()
end
