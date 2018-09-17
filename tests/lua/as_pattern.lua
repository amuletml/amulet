do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  bottom(function(ca)
    local b = ca.a
    return b.a + b.b + ca.c
  end)
end
