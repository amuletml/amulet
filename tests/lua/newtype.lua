do
  local function It(x) return { __tag = "It", x } end
  local function Mk(x) return { __tag = "Mk", x } end
  local function Foo(gl) return gl end
  (nil)(Foo)
  (nil)(function(gm) return gm end)
  (nil)(It)
  (nil)(Mk)
end
