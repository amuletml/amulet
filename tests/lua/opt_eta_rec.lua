do
  local function f(x) return f(x) end
  f(nil)
end
