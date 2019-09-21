do
  local function main(x)
    local __n = {}
    for k, v in pairs(x) do
      __n[k] = v
    end
    __n.x = 1
  end
  (nil)(main)
end
