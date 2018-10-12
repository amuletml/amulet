do
  local bottom = nil
  local function main(bq) return (bq.a + bq.b) * bq.c end
  bottom(main)
end
