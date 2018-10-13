do
  local function main(au)
    local x = au.x
    return x + main({ x = x })
  end
  main({ x = 1 })
end
