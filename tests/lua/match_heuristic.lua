do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function common_prefix(j)
    local et = j._1
    local es = j._2
    if et == 1 then
      if es == 2 then
        return "foo"
      elseif es == 3 then
        return "bar"
      else
        return error("Pattern matching failure in match expression at match_heuristic.ml[4:21 ..4:28]")
      end
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[4:21 ..4:28]")
    end
  end
  local function common_suffix(k)
    local ex = k._1
    local ew = k._2
    if ex == 1 then
      if ew == 2 then
        return "foo"
      elseif ew == 3 then
        return "bar"
      else
        return error("Pattern matching failure in match expression at match_heuristic.ml[8:21 ..8:28]")
      end
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[8:21 ..8:28]")
    end
  end
  local function mixed_1(l)
    local fa = l._2
    local fe, fd = fa._1, fa._2
    if l._1 then
      if fe == 1 then
        return 1
      else
        if fd.__tag == "Cons" then
          return 3
        else
          return error("Pattern matching failure in match expression at match_heuristic.ml[13:15 ..13:22]")
        end
      end
    else
      if fd.__tag == "Nil" then
        if fe == 2 then
          return 2
        else
          return error("Pattern matching failure in match expression at match_heuristic.ml[13:15 ..13:22]")
        end
      elseif fd.__tag == "Cons" then
        return 3
      end
    end
  end
  local bottom = nil
  bottom({ common_prefix = common_prefix, common_suffix = common_suffix, mixed_1 = mixed_1 })
end
