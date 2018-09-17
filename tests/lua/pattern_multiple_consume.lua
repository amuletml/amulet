do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local main
  main = function(at)
    local x = at.x
    return x + main({ x = x })
  end
  main({ x = 1 })
end
