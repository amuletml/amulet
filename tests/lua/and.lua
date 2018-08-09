do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local function __builtin_force (x)
    if x[2.0] then
      return x[1.0]
    else
      x[1.0], x[2.0] = x[1.0](__builtin_unit), true
      return x[1.0]
    end
  end
  local function __builtin_Lazy (x)
    return {
      [1.0] = x,
      [2.0] = false,
      __tag = "lazy"
    }
  end
  local function main (f)
    if f(1) then
      local ca = __builtin_Lazy(function (bx)
        return f(2)
      end)
      local bt = __builtin_force
      return bt(ca)
    else
      return false
    end
  end
  main()
end
