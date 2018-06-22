do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local function main (f)
    return function (g)
      local a = f(1)
      local b = f(2)
      local c = f(3)
      return g(b)(c)(a)
    end
  end
  main()()
end
