do
  local Nil = { __tag = "Nil" }
  local function filter(f, x)
    if x.__tag == "Nil" then
      return Nil
    end
    local tmp = x[1]
    local x0, xs = tmp._1, tmp._2
    if f(x0) then
      return { { _1 = x0, _2 = filter(f, xs) }, __tag = "Cons" }
    end
    return filter(f, xs)
  end
  local function filter0(f) return function(x) return filter(f, x) end end
  (nil)(filter0)
end
