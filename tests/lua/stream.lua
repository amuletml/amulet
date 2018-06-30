do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local function _dot_dot (l)
    return function (r)
      return l .. r
    end
  end
  local io_write = io.write
  local print = print
  local to_string = tostring
  local function Skip (x)
    return {
      __tag = "Skip",
      [1] = x
    }
  end
  local function Yield (x)
    return {
      __tag = "Yield",
      [1] = x
    }
  end
  local Done = {
    __tag = "Done"
  }
  local function Stream (x)
    return {
      __tag = "Stream",
      [1] = x
    }
  end
  local main = (function ()
    local go
    local nt = to_string
    go = function (st)
      if st > 5 then
        return print("]")
      else
        local nd = nt(st)
        local nc = _dot_dot("'")
        io_write(nc(nd .. "', "))
        return go(st + 1)
      end
    end
    io_write("[")
    return go(1)
  end)()
end
