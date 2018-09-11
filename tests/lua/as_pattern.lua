do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  bottom(function(bx)
    local b = bx.a
    return b.a + b.b + bx.c
  end)
end
