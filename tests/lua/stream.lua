do
  local io_write = io.write
  local print = print
  local to_string = tostring
  local function go(st)
    if st > 5 then
      return print("]")
    end
    io_write("'" .. to_string(st) .. "', ")
    return go(st + 1)
  end
  io_write("[")
  local main = go(1)
  (nil)(main)
end
