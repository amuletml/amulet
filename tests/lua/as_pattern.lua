do
  local function main(g)
    local b = g.a
    return b.a + b.b + g.c
  end
  (nil)(main)
end
