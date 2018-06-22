do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local function main (f)
    local a = f(1)
    local b = f(2)
    local c = f(3)
    return {
      _1 = b,
      _2 = {
        _1 = c,
        _2 = a
      }
    }
  end
  main()
end
