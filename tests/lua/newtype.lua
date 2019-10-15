do
  local function It(a) return { __tag = "It", a } end
  local function Mk(a) return { __tag = "Mk", a } end
  local function Foo(tmp) return tmp end
  (nil)(Foo);
  (nil)(function(tmp) return tmp end);
  (nil)(It);
  (nil)(Mk)
end
