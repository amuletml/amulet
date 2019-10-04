do
  local function main(x)
    local b = x.a
    return b.a + b.b + x.c
  end
  (nil)(main)
end
