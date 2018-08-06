do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local dr = {
    _1 = function (x)
      return x
    end,
    _2 = function (x)
      return x
    end
  }
  local d = dr._1
  local e = dr._2
  local main = {
    a = 3,
    b = 5,
    c = 6,
    d = d,
    e = e
  }
end
