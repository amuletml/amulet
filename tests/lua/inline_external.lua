do
  local _print = print
  local r = { 0 }
  _print(r[1])
  r[1] = 1
  _print(r[1])
end
