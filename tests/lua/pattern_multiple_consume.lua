do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local main
  main = function(ao)
    local x = ao.x
    return x + main({ x = x })
  end
  main({ x = 1 })
end
