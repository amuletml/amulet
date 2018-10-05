do
  local function main(x)
    if 0 == x then
      return 1
    else
      return 4
    end
  end
  local bottom = nil
  bottom(main)
end
