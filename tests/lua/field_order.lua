do
  local bottom = print
  bottom(function(f) return { a = f(1), b = f(2) } end)
end
