do
  local Nil = { __tag = "Nil" }
  (nil)({
    { _1 = 1, _2 = { { _1 = 2, _2 = { { _1 = 3, _2 = Nil }, __tag = "Cons" } }, __tag = "Cons" } },
    __tag = "Cons"
  })
  (nil)(1)
  (nil)({ { _1 = 2, _2 = Nil }, __tag = "Cons" })
  local function k(xss)
    if xss.__tag ~= "Cons" then
      return Nil
    end
    local tmp = xss[1]
    local x, xs = tmp._1, tmp._2
    local function o(xss0)
      if xss0.__tag ~= "Cons" then
        return k(xs)
      end
      local tmp0 = xss0[1]
      return { { _2 = o(tmp0._2), _1 = { _1 = x, _2 = tmp0._1 } }, __tag = "Cons" }
    end
    return o({
      { _1 = 4, _2 = { { _1 = 5, _2 = { { _1 = 6, _2 = Nil }, __tag = "Cons" } }, __tag = "Cons" } },
      __tag = "Cons"
    })
  end
  (nil)(k({
    { _1 = 1, _2 = { { _1 = 2, _2 = { { _1 = 3, _2 = Nil }, __tag = "Cons" } }, __tag = "Cons" } },
    __tag = "Cons"
  }))
  local function s(xss)
    if xss.__tag ~= "Cons" then
      return Nil
    end
    local tmp = xss[1]
    local x, xs = tmp._1, tmp._2
    local b = x + 1
    local function w(xss0)
      if xss0.__tag == "Cons" then
        return { { _1 = { _1 = x, _2 = b }, _2 = w(xss0[1]._2) }, __tag = "Cons" }
      end
      return s(xs)
    end
    return w({ { _1 = b, _2 = { { _1 = x, _2 = Nil }, __tag = "Cons" } }, __tag = "Cons" })
  end
  (nil)(s({
    { _1 = 1, _2 = { { _1 = 2, _2 = { { _1 = 3, _2 = Nil }, __tag = "Cons" } }, __tag = "Cons" } },
    __tag = "Cons"
  }))
end
