do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local Mono = {
    __tag = "Mono"
  }
  local go = nil
  local main = go(__builtin_unit)
  local function Mono0 (x)
    return {
      __tag = "Mono0",
      [1] = x
    }
  end
  local go0 = nil
  local main0 = (function ()
    go0(__builtin_unit)
    return Mono0(2)
  end)()
  local bottom = nil
  local cu = bottom({
    _1 = main,
    _2 = main0
  })
end
