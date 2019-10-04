do
  local function main(f)
    local tmp = f(nil)
    return tmp.a + tmp.b
  end
  (nil)(main)
end
