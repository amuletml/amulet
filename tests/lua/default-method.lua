do
  local use = print
  use(function(hn) return "tail" .. "()" end)
end
