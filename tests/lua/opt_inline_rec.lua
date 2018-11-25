do
  local function f(x) return f(x) + 1 + 1 end
  local bottom = nil
  (nil)(f)
end
