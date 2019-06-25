do
  local function It(x) return { __tag = "It", x } end
  local function Mk(x) return { __tag = "Mk", x } end
  local function Foo(fs) return fs end
  (nil)(Foo)
  (nil)(function(ft) return ft end)
  (nil)(It)
  (nil)(Mk)
end
