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
    local no = k[1]
    local m, l = no._1, no._2
    local function n(o)
      if o.__tag ~= "Cons" then
        return j(l)
      end
      local mv = o[1]
      return Cons({ _2 = n(mv._2), _1 = { _1 = m, _2 = mv._1 } })
    end
    return n(Cons({ _1 = 4, _2 = Cons({ _1 = 5, _2 = Cons({ _1 = 6, _2 = Nil }) }) }))
  end
  (nil)(j(Cons({ _1 = 1, _2 = Cons({ _1 = 2, _2 = Cons({ _1 = 3, _2 = Nil }) }) })))
  local function r(s)
    if s.__tag ~= "Cons" then
      return Nil
    end
    local pl = s[1]
    local u, t = pl._1, pl._2
    local b = u + 1
    local function v(w)
      if w.__tag == "Cons" then
        return Cons({ _1 = { _1 = u, _2 = b }, _2 = v(w[1]._2) })
      end
      return r(t)
    end
    return v(Cons({ _1 = b, _2 = Cons({ _1 = u, _2 = Nil }) }))
  end
  (nil)(r(Cons({ _1 = 1, _2 = Cons({ _1 = 2, _2 = Cons({ _1 = 3, _2 = Nil }) }) })))
end
