do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function common_prefix(j)
    local et = j._1
    local es = j._2
    if 1 == et then
      if 2 == es then
        return "foo"
      elseif 3 == es then
        return "bar"
      else
        return error("Pattern matching failure in match expression at match_heuristic.ml[4:21 ..4:28]")
      end
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[4:21 ..4:28]")
    end
  end
  local function common_suffix(k)
    local ez = k._1
    local ey = k._2
    if 1 == ez then
      if 2 == ey then
        return "foo"
      elseif 3 == ey then
        return "bar"
      else
        return error("Pattern matching failure in match expression at match_heuristic.ml[8:21 ..8:28]")
      end
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[8:21 ..8:28]")
    end
  end
  local function mixed_1(l)
    local fe = l._2
    local fh = fe._2
    local fi = fe._1
    if l._1 then
      if 1 == fi then
        return 1
      else
        if fh.__tag == "Cons" then
          return 3
        else
          return error("Pattern matching failure in match expression at match_heuristic.ml[13:15 ..13:22]")
        end
      end
    else
      if fh.__tag == "Nil" then
        if 2 == fi then
          return 2
        else
          return error("Pattern matching failure in match expression at match_heuristic.ml[13:15 ..13:22]")
        end
      elseif fh.__tag == "Cons" then
        return 3
      end
    end
  end
  local bottom = nil
  bottom({ common_prefix = common_prefix, common_suffix = common_suffix, mixed_1 = mixed_1 })
end
