do
  local function It(x) return { __tag = "It", x } end
  local function Foo(fv) return fv end
  (nil)(Foo)
  (nil)(function(fw) return fw end)
  (nil)(It)
  (nil)(function(fx) return fx end)
end
