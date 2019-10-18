do
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
  (nil)({
    _2 = function(x) return __builtin_force(x) end,
    _1 = { function(tmp) return 2 end, false, __tag = "lazy" }
  })
end
