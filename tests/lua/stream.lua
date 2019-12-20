do
  local io_write = io.write
  local _print = print
  local to_string = tostring
  io_write("[")
  local function go(st)
    if st > 5 then return _print("]") end
    local tmp = st + 1
    io_write("'" .. to_string(st) .. "', ")
    return go(tmp)
  end
  local main = go(1);
  (nil)(main)
end
