do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local function __builtin_force(x)
    if x[2] then
      return x[1]
    else
      local thunk = x[1]
      x[1] = function()
        error("loop while forcing thunk")
      end
      x[1] = thunk(__builtin_unit)
      x[2] = true
      return x[1]
    end
  end
  local function __builtin_Lazy(x) return { x, false, __tag = "lazy" } end
  local bottom = nil
  if bottom(1) then
    bottom(__builtin_force(__builtin_Lazy(function(cq) return bottom(2) end)))
  else
    bottom(false)
  end
end
