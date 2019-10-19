do
  local use = print
  use(function(ge) return "tail" .. "()" end)
end
