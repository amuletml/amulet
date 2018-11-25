do
  local None = { __tag = "None" }
  local function Some(x) return { __tag = "Some", x } end
  local bottom = nil
  local a = (nil)(1)
  if (nil).__tag == "None" then
    (nil)((nil)(a))
  elseif (nil).__tag == "Some" then
    (nil)((nil)(a + (nil)[1] * 2))
  end
end
