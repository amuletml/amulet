do
  local function main(ax)
    local x = ax.x
    return x + main({ x = x })
  end
  main({ x = 1 })
end
