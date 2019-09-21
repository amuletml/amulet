do
  local function main(f)
    local a = f(1)
    return { _1 = f(2), _2 = { _2 = a, _1 = f(3) } }
  end
  (nil)(main)
end
