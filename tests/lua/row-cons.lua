do
  local function __builtin_extend(key, value, record)
    local new = {}
    for k, v in pairs(record) do
      new[k] = v
    end
    new[key] = value
    return new
  end
  __builtin_extend("foo", 1, __builtin_extend("bar", true, {}))
end
