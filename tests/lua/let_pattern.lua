do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local de = {
    _1 = function (x)
      return x
    end,
    _2 = function (x)
      return x
    end
  }
  local d = de._1
  local e = de._2
  local bottom = nil
  local dx = bottom({
    a = 3,
    b = 5,
    c = 6,
    d = d,
    e = e
  })
end
