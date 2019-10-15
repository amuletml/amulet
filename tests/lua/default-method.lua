do
  local use = print
  use(function(fu) return "tail" .. "()" end)
end
