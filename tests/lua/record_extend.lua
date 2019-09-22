do
  local function __builtin_clone(record)
    local new = {}
    for k, v in pairs(record) do
      new[k] = v
    end
    return new
  end
  local function main(x)
    local __n = __builtin_clone(x)
    __n.x = 1
    return __n
  end
  (nil)(main)
end
