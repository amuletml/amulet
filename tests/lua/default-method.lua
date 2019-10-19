do
  local use = print
  use(function(fy) return "tail" .. "()" end)
end
