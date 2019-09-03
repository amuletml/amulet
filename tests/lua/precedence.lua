do
  local function main(fi)
    local a, b, c = fi.a, fi.b, fi.c
    (nil)((a + b) * c)
    (nil)(a * (b + c))
    (nil)(a ^ b * c)
    (nil)(a * b ^ c)
    (nil)(a * (b / c))
    return (nil)(a * b / c)
  end
  (nil)(main)
end
