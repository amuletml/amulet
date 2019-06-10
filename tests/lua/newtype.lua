do
  local function It(x) return { __tag = "It", x } end
  local function Foo(fq) return fq end
  (nil)(Foo)
  (nil)(function(fr) return fr end)
  (nil)(It)
  (nil)(function(fs) return fs end)
end
