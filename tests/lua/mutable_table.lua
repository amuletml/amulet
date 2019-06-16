do
  local empty = function() return {} end
  local t = empty(nil)
  t.foo = "foo"
end
