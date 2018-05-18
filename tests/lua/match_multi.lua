do
  local function main (f)
    local am = f(nil)
    return am.a + am.b
  end
  main()
end
