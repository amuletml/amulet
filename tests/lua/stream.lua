do
  local io_write = io.write
  local print = print
  local to_string = tostring
  io_write("[")
  local function go(st)
    if st > 5 then
      return print("]")
    end
    io_write("'" .. to_string(st) .. "', ")
    return go(st + 1)
  end
  local main = go(1)
  (nil)(main)
end
