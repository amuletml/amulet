do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local function __builtin_trap()
    error("Loop while forcing thunk")
  end
  local function __builtin_force(x)
    if x[2] then
      return x[1]
    else
      local thunk = x[1]
      x[1] = __builtin_trap
      x[1] = thunk()
      x[2] = true
      return x[1]
    end
  end
  local function __builtin_Lazy(x) return { x, false, __tag = "lazy" } end
  (nil)({
    _2 = function(x) return __builtin_force(x) end,
    _1 = __builtin_Lazy(function(ab) return 2 end)
  })
end
