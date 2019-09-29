do
  local function f(i) return i end
  local function g(j) return j end
  (nil)({ f = f, g = g })
end
