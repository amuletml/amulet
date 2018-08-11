do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local function _plus0 (bt)
    return function (bu)
      return bt + bu
    end
  end
  local main = {
    _1 = _plus0,
    _2 = {
      _1 = function (bx)
        return 2 * bx
      end,
      _2 = {
        _1 = function (d)
          return d / 2
        end,
        _2 = function (e)
          return e.foo
        end
      }
    }
  }
  local bottom = nil
  local bp = bottom(main)
end
