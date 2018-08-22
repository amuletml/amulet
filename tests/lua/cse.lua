do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local print = print
  local function Foo (x)
    return {
      __tag = "Foo",
      [1] = x
    }
  end
  local ch = (function ()
    local x = Foo(1)
    local cd = print
    cd(x)
    return cd(x)
  end)()
end
