do
  local function main(fa)
    local a, b, c = fa.a, fa.b, fa.c
    (nil)((a + b) * c)
    (nil)(a * (b + c))
    (nil)(a ^ b * c)
    (nil)(a * b ^ c)
    (nil)(a * (b / c))
    return (nil)(a * b / c)
  end
  (nil)(main)
end
