do
  local use = print
  use(function(hc) return "tail" .. "()" end)
end
