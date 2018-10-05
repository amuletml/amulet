do
  local function main(cb)
    local b = cb.a
    return b.a + b.b + cb.c
  end
  local bottom = nil
  bottom(main)
end
