do
  local None = { __tag = "None" }
  local function Some(x) return { __tag = "Some", x } end
  local bottom = nil
  local eb = nil
  local ea = nil
  local ed = nil
  local a = eb(1)
  if ed.__tag == "None" then
    ea(eb(a))
  elseif ed.__tag == "Some" then
    ea(eb(a + ed[1] * 2))
  end
end
