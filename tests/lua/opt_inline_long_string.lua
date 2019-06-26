do
  local print = print
  if "123456789" == "123456789" then
    print("123456789")
  else
    error("Pattern matching failure in let expression at opt_inline_long_string.ml[10:7 ..10:22]")
  end
end
