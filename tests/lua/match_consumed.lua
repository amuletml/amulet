do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local None = {
    __tag = "None"
  }
  local function Some (x)
    return {
      __tag = "Some",
      [1] = x
    }
  end
  local function main (f)
    return function (x)
      local a = f(1)
      if x.__tag == "None" then
        return f(a)
      elseif x.__tag == "Some" then
        return f(a + x[1] * 2)
      end
    end
  end
  main()()
end
