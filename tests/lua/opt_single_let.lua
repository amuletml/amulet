do
  local function f(x) return x(nil) end
  local function g(x) return x(nil) end
  (nil)({ f = f, g = g })
end
