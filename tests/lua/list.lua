do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  (nil)(Cons({ _1 = 1, _2 = Cons({ _1 = 2, _2 = Cons({ _1 = 3, _2 = Nil }) }) }))
  (nil)(1)
  (nil)(Cons({ _1 = 2, _2 = Nil }))
  local function j(xss)
    if xss.__tag ~= "Cons" then
      return Nil
    end
    local tmp = xss[1]
    local x, xs = tmp._1, tmp._2
    local function n(xss0)
      if xss0.__tag ~= "Cons" then
        return j(xs)
      end
      local tmp0 = xss0[1]
      return Cons({ _2 = n(tmp0._2), _1 = { _1 = x, _2 = tmp0._1 } })
    end
    return n(Cons({ _1 = 4, _2 = Cons({ _1 = 5, _2 = Cons({ _1 = 6, _2 = Nil }) }) }))
  end
  (nil)(j(Cons({ _1 = 1, _2 = Cons({ _1 = 2, _2 = Cons({ _1 = 3, _2 = Nil }) }) })))
  local function r(xss)
    if xss.__tag ~= "Cons" then
      return Nil
    end
    local tmp = xss[1]
    local x, xs = tmp._1, tmp._2
    local b = x + 1
    local function v(xss0)
      if xss0.__tag == "Cons" then
        return Cons({ _1 = { _1 = x, _2 = b }, _2 = v(xss0[1]._2) })
      end
      return r(xs)
    end
    return v(Cons({ _1 = b, _2 = Cons({ _1 = x, _2 = Nil }) }))
  end
  (nil)(r(Cons({ _1 = 1, _2 = Cons({ _1 = 2, _2 = Cons({ _1 = 3, _2 = Nil }) }) })))
end
