do
  local __builtin_unit = {
    ["__tag"] = "__builtin_unit"
  }
  local function main (x)
    local __o, __n = x, {
      
    }
    for k, v in pairs(__o) do
      __n[k] = v
    end
    __n.x = 1.0
    return __n
  end
  main()
end
