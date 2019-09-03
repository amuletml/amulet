do
  local function It(x) return { __tag = "It", x } end
  local function Mk(x) return { __tag = "Mk", x } end
  local function Foo(gi) return gi end
  (nil)(Foo)
  (nil)(function(gj) return gj end)
  (nil)(It)
  (nil)(Mk)
end
