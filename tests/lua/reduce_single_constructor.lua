do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local Mono = { __tag = "Mono" }
  local go = nil
  local main = go(__builtin_unit)
  local function Mono0(x) return { __tag = "Mono0", x } end
  local go0 = nil
  go0(__builtin_unit)
  local main0 = Mono0(2)
  local bottom = nil
  bottom({ _1 = main, _2 = main0 })
end
