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
  local fb = (function ()
    local fa = bottom
    fa(Foo)
    fa(Bar)
    fa(It)
    return fa(Mk)
  end)()
end
