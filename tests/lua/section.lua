do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local function _plus0 (au)
    return function (av)
      return au + av
    end
  end
  local main = {
    _1 = _plus0,
    _2 = {
      _1 = function (ay)
        return 2 * ay
      end,
      _2 = {
        _1 = function (b)
          return b / 2
        end,
        _2 = function (c)
          return c.foo
        end
      }
    }
  }
end
