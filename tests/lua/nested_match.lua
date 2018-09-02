do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local bottom = nil
  bottom(function(h)
    local cs = h._1
    local cr = h._2
    if cs.__tag == "Nil" then
      return 1
    elseif cs.__tag == "Cons" then
      if cr.__tag == "Nil" then
        return 2
      elseif cr.__tag == "Cons" then
        return 3
      end
    else
      if cr.__tag == "Nil" then
        return 2
      else
        return error("Pattern matching failure in match expression at nested_match.ml[3:12 ..3:19]")
      end
    end
  end)
end
