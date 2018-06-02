do
  local None = {
    [1.0] = "None"
  }
  local function Some (x)
    return {
      ["__tag"] = "Some",
      [2.0] = x
    }
  end
  local function main (f)
    return function (x)
      local a = f(1.0)
      local b
      if x.__tag == "None" then
        b = 0.0
      elseif x.__tag == "Some" then
        b = x[2.0] * 2.0
      end
      return f(a + b)
    end
  end
  main()()
end
