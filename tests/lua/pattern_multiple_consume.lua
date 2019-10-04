do
  local function main(tmp)
    local x = tmp.x
    return x + main({ x = x })
  end
  main({ x = 1 })
end
