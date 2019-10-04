do
  local function main(ay)
    local x = ay.x
    return x + main({ x = x })
  end
  main({ x = 1 })
end
