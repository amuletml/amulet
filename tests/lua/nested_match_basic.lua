do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function zip(im_1, im_2, im_3)
    if im_2.__tag == "Nil" then
      return Cons({ _1 = 1, _2 = Nil })
    elseif im_2.__tag == "Cons" then
      local hg = im_2[1]
      if im_3.__tag == "Nil" then
        return Cons({ _1 = 2, _2 = Nil })
      elseif im_3.__tag == "Cons" then
        local hh = im_3[1]
        return Cons({ _1 = im_1(hg._1)(hh._1), _2 = zip(im_1, hg._2, hh._2) })
      end
    end
  end
  local function zip0(f) return function(xs) return function(ys) return zip(f, xs, ys) end end end
  local bottom = nil
  bottom(zip0)
end
