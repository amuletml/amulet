do
  local function main (f)
    local ao = f(nil)
    return ao.a + ao.b
  end
  main()
end
