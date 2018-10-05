do
  local function __builtin_Lazy(x) return { x, false, __tag = "lazy" } end
  local __builtin_unit = { __tag = "__builtin_unit" }
  local function __builtin_force_err()
    error("Loop while forcing thunk")
  end
  local function __builtin_force(x)
    if x[2] then
      return x[1]
    else
      local thunk = x[1]
      x[1] = __builtin_force_err
      x[1] = thunk(__builtin_unit)
      x[2] = true
      return x[1]
    end
  end
  local bottom = nil
  bottom({
    _1 = __builtin_Lazy(function(aa) return 2 end),
    _2 = function(x) return __builtin_force(x) end
  })
end
