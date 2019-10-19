do
  local use = print
  use(function(fw) return "tail" .. "()" end)
end
