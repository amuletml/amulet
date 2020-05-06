do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { x, __tag = "Cons" } end
  local _greater_greater_equals = bind
  local pure = pure
  _greater_greater_equals({
    { _1 = 1, _2 = { { _1 = 2, _2 = { { _1 = 3, _2 = Nil }, __tag = "Cons" } }, __tag = "Cons" } },
    __tag = "Cons"
  }, function(x)
    return _greater_greater_equals(Cons({
      _1 = 4,
      _2 = Cons({ _1 = 5, _2 = Cons({ _1 = 6, _2 = Nil }) })
    }), function(y) return pure({ _1 = x, _2 = y }) end)
  end)
end
