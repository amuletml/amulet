do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local function Foo(x) return { __tag = "Foo", x } end
  local function Bar(x) return { __tag = "Bar", x } end
  local function It(x) return { __tag = "It", x } end
  local function Mk(x) return { __tag = "Mk", x } end
  local bottom = nil
  bottom(Foo)
  bottom(Bar)
  bottom(It)
  bottom(Mk)
end
