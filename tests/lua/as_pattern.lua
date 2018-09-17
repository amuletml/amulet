do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  bottom(function(ce)
    local b = ce.a
    return b.a + b.b + ce.c
  end)
end
