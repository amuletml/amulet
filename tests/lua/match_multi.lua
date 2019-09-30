do
  local function main(f)
    local bh = f(nil)
    return bh.a + bh.b
  end
  (nil)(main)
end
