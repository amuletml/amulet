do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local function _plus0 (bv)
    return function (bw)
      return bv + bw
    end
  end
  local main = {
    _1 = _plus0,
    _2 = {
      _1 = function (bz)
        return 2 * bz
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
  local br = bottom(main)
end
