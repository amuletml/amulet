do
  local function main (f)
    local ak = f(nil)
    return ak.a + ak.b
  end
  main()
end
