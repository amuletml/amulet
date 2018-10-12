do
  local function main(ce)
    local b = ce.a
    return b.a + b.b + ce.c
  end
  local bottom = nil
  bottom(main)
end
