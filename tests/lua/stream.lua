do
  local io_write = io.write
  local print = print
  local to_string = tostring
  local function Skip(x) return { __tag = "Skip", x } end
  local function Yield(x) return { __tag = "Yield", x } end
  local Done = { __tag = "Done" }
  local function Stream(x) return { __tag = "Stream", x } end
  local function go(st)
    if st > 5 then
      return print("]")
    end
    io_write("'" .. to_string(st) .. "', ")
    return go(st + 1)
  end
  io_write("[")
  local main = go(1)
  local bottom = nil
  bottom(main)
end
