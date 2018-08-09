do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local function Foo (x)
    return {
      __tag = "Foo",
      [1] = x
    }
  end
  local function Bar (x)
    return {
      __tag = "Bar",
      [1] = x
    }
  end
  local function It (x)
    return {
      __tag = "It",
      [1] = x
    }
  end
  local function Mk (x)
    return {
      __tag = "Mk",
      [1] = x
    }
  end
  local bottom = nil
  local fa = (function ()
    local x = bottom
    x(Foo)
    x(Bar)
    x(It)
    return x(Mk)
  end)()
end
