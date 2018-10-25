do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function filter(gi_1, gi_2)
    if gi_2.__tag == "Nil" then
      return Nil
    elseif gi_2.__tag == "Cons" then
      local fq = gi_2[1]
      local x = fq._1
      local xs = fq._2
      if gi_1(x) then
        return Cons({ _1 = x, _2 = filter(gi_1, xs) })
      else
        return filter(gi_1, xs)
      end
    end
  end
  local function filter0(f) return function(o) return filter(f, o) end end
  local bottom = nil
  bottom(filter0)
end
