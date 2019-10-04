do
  local function It(a) return { __tag = "It", a } end
  local function Mk(a) return { __tag = "Mk", a } end
  local function Foo(gp) return gp end
  (nil)(Foo)
  (nil)(function(gq) return gq end)
  (nil)(It)
  (nil)(Mk)
end
