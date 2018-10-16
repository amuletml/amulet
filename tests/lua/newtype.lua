do
  local function Foo(fr) return fr end
  local function It(x) return { __tag = "It", x } end
  local bottom = nil
  bottom(Foo)
  bottom(function(fs) return fs end)
  bottom(It)
  bottom(function(ft) return ft end)
end
