do
  local function __builtin_clone(record)
    local new = {}
    for k, v in pairs(record) do
      new[k] = v
    end
    return new
  end
  (nil)(function(x)
    local __n = __builtin_clone(x)
    __n.c = 2
    __n.b = 5
    __n.a = 3
    return __n
  end);
  (nil)(function(x)
    local x0 = __builtin_clone(x)
    x0.a = 1
    return { _1 = x0, _2 = x0 }
  end);
  (nil)(function(x)
    local iz = __builtin_clone(x)
    iz.a = 2
    local x0 = __builtin_clone(x)
    x0.a = 1
    return { _1 = x0, _2 = iz }
  end)
end
