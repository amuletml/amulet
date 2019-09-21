do
  local function main(f)
    if f(1) then
      return f(2)
    end
    return f(3)
  end
  (nil)(main)
end
