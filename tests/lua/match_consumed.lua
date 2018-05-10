do
  local None = {
    [1.0] = "None"
  }
  local function Some (x)
    return {
      [1.0] = "Some",
      [2.0] = x
    }
  end
  local function main (f)
    return function (x)
      local a = f(1.0)
      local b
      if x[1.0] == "None" then
        b = 0.0
      elseif x[1.0] == "Some" then
        b = x[2.0] * 2.0
      end
      return f(a + b)
    end
  end
  main()()
end
