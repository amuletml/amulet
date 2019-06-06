do
  local function main(cf)
    local b = cf.a
    return b.a + b.b + cf.c
  end
  (nil)(main)
end
