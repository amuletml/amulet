do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local function _star (l)
    return function (r)
      return l * r
    end
  end
  local function _plus (l)
    return function (r)
      return l + r
    end
  end
  local main = {
    _1 = _plus,
    _2 = {
      _1 = _star(2),
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
