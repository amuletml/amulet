do
  local function f(x) return x end
  local Z = { __tag = "Z" }
  local function g(x) return x end
  local bottom = nil
  (nil)({ f = f, g = g })
end
