do
  local function main(av)
    local x = av.x
    return x + main({ x = x })
  end
  main({ x = 1 })
end
