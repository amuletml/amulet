do
  local __builtin_unit = {
    ["__tag"] = "__builtin_unit"
  }
  local function main (f)
    return function (g)
      local a = f(1.0)
      local b = f(2.0)
      local c = f(3.0)
      return g(b)(c)(a)
    end
  end
  main()()
end
