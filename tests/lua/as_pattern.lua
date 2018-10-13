do
  local function main(cg)
    local b = cg.a
    return b.a + b.b + cg.c
  end
  local bottom = nil
  bottom(main)
end
