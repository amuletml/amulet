do
  local use = print
  use(function(hf) return "tail" .. "()" end)
end
