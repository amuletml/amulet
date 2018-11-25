do
  local set = function(x, y)
    x[1] = y
  end
  local print = print
  local r = { 0 }
  print(r[1])
  set(r, 1)
  print(r[1])
end
