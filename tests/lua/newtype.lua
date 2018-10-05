do
  local function Foo(fj) return fj end
  local function It(x) return { __tag = "It", x } end
  local bottom = nil
  bottom(Foo)
  bottom(function(fk) return fk end)
  bottom(It)
  bottom(function(fl) return fl end)
end
