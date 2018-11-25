do
  local function main(ch)
    local b = ch.a
    return b.a + b.b + ch.c
  end
  local bottom = nil
  (nil)(main)
end
