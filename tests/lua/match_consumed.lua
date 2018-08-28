do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local None = { __tag = "None" }
  local function Some(x) return { __tag = "Some", x } end
  local bottom = nil
  local a = bottom(1)
  if bottom.__tag == "None" then
    bottom(bottom(a))

  elseif bottom.__tag == "Some" then
    bottom(bottom(a + bottom[1] * 2))
  end
end
