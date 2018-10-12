do
  local function main(at)
    local x = at.x
    return x + main({ x = x })
  end
  main({ x = 1 })
end
