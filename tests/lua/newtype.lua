do
  local function It(x) return { __tag = "It", x } end
  local function Foo(fn) return fn end
  (nil)(Foo)
  (nil)(function(fo) return fo end)
  (nil)(It)
  (nil)(function(fp) return fp end)
end
