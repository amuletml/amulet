do
  local function main(fo)
    local a, b, c = fo.a, fo.b, fo.c
    (nil)((a + b) * c)
    (nil)(a * (b + c))
    (nil)(a ^ b * c)
    (nil)(a * b ^ c)
    (nil)(a * (b / c))
    return (nil)(a * b / c)
  end
  (nil)(main)
end
