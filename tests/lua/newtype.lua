do
  local function Foo(fp) return fp end
  local function It(x) return { __tag = "It", x } end
  local bottom = nil
  bottom(Foo)
  bottom(function(fq) return fq end)
  bottom(It)
  bottom(function(fr) return fr end)
end
