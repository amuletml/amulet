do
  local function main(aq)
    local x = aq.x
    return x + main({ x = x })
  end
  main({ x = 1 })
end
