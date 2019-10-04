do
  local function f(x) return x end
  local function g(x) return x end
  (nil)({ f = f, g = g })
end
