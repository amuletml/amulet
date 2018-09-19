do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  bottom(function(cb)
    local b = cb.a
    return b.a + b.b + cb.c
  end)
end
