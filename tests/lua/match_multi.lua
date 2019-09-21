do
  local function main(f)
    local bm = f(nil)
    return bm.a + bm.b
  end
  (nil)(main)
end
