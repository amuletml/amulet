do
  local use = print
  use(function(hl) return "tail" .. "()" end)
end
