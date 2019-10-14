do
  local function main(x)
    if x == 0 then return 1 end
    return 4
  end
  (nil)(main)
end
