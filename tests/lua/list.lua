do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  (nil)(Cons({ _1 = 1, _2 = Cons({ _1 = 2, _2 = Cons({ _1 = 3, _2 = Nil }) }) }))
  (nil)(1)
  (nil)(Cons({ _1 = 2, _2 = Nil }))
  local function j(k)
    if k.__tag == "Cons" then
      local of = k[1]
      local m, l = of._1, of._2
      local function n(o)
        if o.__tag == "Cons" then
          local np = o[1]
          return Cons({ _2 = n(np._2), _1 = { _1 = m, _2 = np._1 } })
        else
          return j(l)
        end
      end
      return n(Cons({ _1 = 4, _2 = Cons({ _1 = 5, _2 = Cons({ _1 = 6, _2 = Nil }) }) }))
    else
      return Nil
    end
  end
  (nil)(j(Cons({ _1 = 1, _2 = Cons({ _1 = 2, _2 = Cons({ _1 = 3, _2 = Nil }) }) })))
  local function r(s)
    if s.__tag == "Cons" then
      local pt = s[1]
      local u, t = pt._1, pt._2
      local b = u + 1
      local function v(w)
        if w.__tag == "Cons" then
          return Cons({ _2 = v(w[1]._2), _1 = { _1 = u, _2 = b } })
        else
          return r(t)
        end
      end
      return v(Cons({ _1 = b, _2 = Cons({ _1 = u, _2 = Nil }) }))
    else
      return Nil
    end
  end
  (nil)(r(Cons({ _1 = 1, _2 = Cons({ _1 = 2, _2 = Cons({ _1 = 3, _2 = Nil }) }) })))
end
