do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  (nil)(Cons({ _1 = 1, _2 = Cons({ _1 = 2, _2 = Cons({ _1 = 3, _2 = Nil }) }) }))
  (nil)(1)
  (nil)(Cons({ _1 = 2, _2 = Nil }))
  local function j(k)
    if k.__tag ~= "Cons" then
      return Nil
    end
    local na = k[1]
    local m, l = na._1, na._2
    local function n(o)
      if o.__tag ~= "Cons" then
        return j(l)
      end
      local ml = o[1]
      return Cons({ _2 = n(ml._2), _1 = { _1 = m, _2 = ml._1 } })
    end
    return n(Cons({ _1 = 4, _2 = Cons({ _1 = 5, _2 = Cons({ _1 = 6, _2 = Nil }) }) }))
  end
  (nil)(j(Cons({ _1 = 1, _2 = Cons({ _1 = 2, _2 = Cons({ _1 = 3, _2 = Nil }) }) })))
  local function r(s)
    if s.__tag ~= "Cons" then
      return Nil
    end
    local om = s[1]
    local u, t = om._1, om._2
    local b = u + 1
    local function v(w)
      if w.__tag == "Cons" then
        return Cons({ _2 = v(w[1]._2), _1 = { _1 = u, _2 = b } })
      end
      return r(t)
    end
    return v(Cons({ _1 = b, _2 = Cons({ _1 = u, _2 = Nil }) }))
  end
  (nil)(r(Cons({ _1 = 1, _2 = Cons({ _1 = 2, _2 = Cons({ _1 = 3, _2 = Nil }) }) })))
end
