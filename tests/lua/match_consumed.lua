do
  local __builtin_unit = {
    ["__tag"] = "__builtin_unit"
  }
  local None = {
    ["__tag"] = "None"
  }
  local function Some (x)
    return {
      ["__tag"] = "Some",
      [1.0] = x
    }
  end
  local function main (f)
    return function (x)
      local a = f(1.0)
      local b
      if x.__tag == "None" then
        b = 0.0
      elseif x.__tag == "Some" then
        b = x[1.0] * 2.0
      end
      return f(a + b)
    end
  end
  main()()
end
