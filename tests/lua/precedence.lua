do
  local function main(ff)
    local a, b, c = ff.a, ff.b, ff.c
    (nil)((a + b) * c)
    (nil)(a * (b + c))
    (nil)(a ^ b * c)
    (nil)(a * b ^ c)
    (nil)(a * (b / c))
    return (nil)(a * b / c)
  end
  (nil)(main)
end
