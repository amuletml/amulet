do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local function Foo(fe) return fe end
  local function It(x) return { __tag = "It", x } end
  local bottom = nil
  bottom(Foo)
  bottom(function(ff) return ff end)
  bottom(It)
  bottom(function(fg) return fg end)
end
