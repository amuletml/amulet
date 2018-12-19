do
  local function Cons(x) return { x, __tag = "Cons" } end
  local Nil = { __tag = "Nil" }
  local function filter(f, k)
    if k.__tag == "Nil" then
      return Nil
    elseif k.__tag == "Cons" then
      local eh = k[1]
      local x, xs = eh._1, eh._2
      if f(x) then
        return Cons({ _1 = x, _2 = filter(f, xs) })
      end
      return filter(f, xs)
    end
  end
  local function filter0(f) return function(k) return filter(f, k) end end
  (nil)(filter0)
end
