do
  local __builtin_unit = {
    ["__tag"] = "__builtin_unit"
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
    ["1"] = _plus,
    ["2"] = {
      ["1"] = _star(2.0),
      ["2"] = {
        ["1"] = function (c)
          return c / 2.0
        end,
        ["2"] = function (d)
          return d.foo
        end
      }
    }
  }
end
