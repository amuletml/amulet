do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local print = print
  local foo = { 1, __tag = "Ref" }
  local function e(ct)
    if not (foo[1] < 10) then
      return __builtin_unit
    end
    foo[1] = foo[1] + 1
    print(foo[1])
    return e(__builtin_unit)
  end
  e(__builtin_unit)
end
