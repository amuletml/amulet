do
  local function main(x)
    if x == 0 then
      return 1
    else
      return 4
    end
  end
  (nil)(main)
end
