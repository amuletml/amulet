do
  local function main(tmp)
    local a, b, c = tmp.a, tmp.b, tmp.c
    (nil)((a + b) * c)
    (nil)(a * (b + c))
    (nil)(a ^ b * c)
    (nil)(a * b ^ c)
    (nil)(a * (b / c))
    return (nil)(a * b / c)
  end
  (nil)(main)
end
