do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  bottom(function(br) return (br.a + br.b) * br.c end)
end
