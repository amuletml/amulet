do
  local function Foo(fv) return fv end
  local function It(x) return { __tag = "It", x } end
  local bottom = nil
  bottom(Foo)
  bottom(function(fw) return fw end)
  bottom(It)
  bottom(function(fx) return fx end)
end
