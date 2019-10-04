do
  local function main(f)
    local bi = f(nil)
    return bi.a + bi.b
  end
  (nil)(main)
end
