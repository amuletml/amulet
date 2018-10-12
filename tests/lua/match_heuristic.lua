do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function common_prefix(j)
    local en = j._1
    local em = j._2
    if 1 == en then
      if 2 == em then
        return "foo"
      elseif 3 == em then
        return "bar"
      else
        return error("Pattern matching failure in match expression at match_heuristic.ml[4:21 ..4:28]")
      end
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[4:21 ..4:28]")
    end
  end
  local function common_suffix(k)
    local et = k._1
    local es = k._2
    if 1 == et then
      if 2 == es then
        return "foo"
      elseif 3 == es then
        return "bar"
      else
        return error("Pattern matching failure in match expression at match_heuristic.ml[8:21 ..8:28]")
      end
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[8:21 ..8:28]")
    end
  end
  local function mixed_1(l)
    local ey = l._2
    local fc = ey._1
    local fb = ey._2
    if l._1 then
      if 1 == fc then
        return 1
      else
        if fb.__tag == "Cons" then
          return 3
        else
          return error("Pattern matching failure in match expression at match_heuristic.ml[13:15 ..13:22]")
        end
      end
    else
      if fb.__tag == "Nil" then
        if 2 == fc then
          return 2
        else
          return error("Pattern matching failure in match expression at match_heuristic.ml[13:15 ..13:22]")
        end
      elseif fb.__tag == "Cons" then
        return 3
      end
    end
  end
  local bottom = nil
  bottom({ common_prefix = common_prefix, common_suffix = common_suffix, mixed_1 = mixed_1 })
end
