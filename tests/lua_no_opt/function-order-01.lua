do
  local file_close = io.close
  local file_read = io.read
  local print = print
  local function file_load(fhandle)
    file_read("*all")
    file_close(fhandle)
    return nil
  end
end
