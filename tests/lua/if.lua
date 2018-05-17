do
  local function main (f)
    if f(1.0) then
      return f(2.0)
    else
      return f(3.0)
    end
  end
  main()
end
