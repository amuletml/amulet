do
  local function It(a) return { __tag = "It", a } end
  local function Mk(a) return { __tag = "Mk", a } end
  local function Foo(gk) return gk end
  (nil)(Foo)
  (nil)(function(gl) return gl end)
  (nil)(It)
  (nil)(Mk)
end
