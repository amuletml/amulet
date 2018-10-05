do
  local function f(x) return x end
  local Z = { __tag = "Z" }
  local function g(x) return x end
  local bottom = nil
  bottom({ f = f, g = g })
end
