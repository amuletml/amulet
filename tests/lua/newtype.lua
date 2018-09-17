do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local function Foo(gf) return gf end
  local function It(x) return { __tag = "It", x } end
  local bottom = nil
  bottom(Foo)
  bottom(function(gg) return gg end)
  bottom(It)
  bottom(function(gh) return gh end)
end
