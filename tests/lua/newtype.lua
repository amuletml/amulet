do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local function Foo (en)
    return en
  end
  local function It (x)
    return {
      __tag = "It",
      [1] = x
    }
  end
  local function main (x)
    x(Foo)
    x(function (eo)
      return eo
    end)
    x(It)
    return x(function (ep)
      return ep
    end)
  end
  main()
end
