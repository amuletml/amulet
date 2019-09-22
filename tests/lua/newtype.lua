do
  local function It(x) return { __tag = "It", x } end
  local function Mk(x) return { __tag = "Mk", x } end
  local function Foo(gm) return gm end
  (nil)(Foo)
  (nil)(function(gn) return gn end)
  (nil)(It)
  (nil)(Mk)
end
