do
  local function f(x) return f(x) + 1 + 1 end
  (nil)(f)
end
