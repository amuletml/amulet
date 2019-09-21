do
  local function main(cg)
    local b = cg.a
    return b.a + b.b + cg.c
  end
  (nil)(main)
end
