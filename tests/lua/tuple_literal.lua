do
  local function main (f)
    local a = f(1.0)
    local b = f(2.0)
    local c = f(3.0)
    return {
      ["1"] = b,
      ["2"] = {
        ["1"] = c,
        ["2"] = a
      }
    }
  end
  main()
end
