do
  local function main (f)
    local al = f(nil)
    return al.a + al.b
  end
  main()
end
