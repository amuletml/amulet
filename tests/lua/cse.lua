do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local print = print
  local function Foo(x) return { __tag = "Foo", x } end
  local x = Foo(1)
  print(x)
  print(x)
end
