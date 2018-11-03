do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function common_prefix(j)
    local eu = j._1
    local et = j._2
    if eu == 1 then
      if et == 2 then
        return "foo"
      elseif et == 3 then
        return "bar"
      else
        return error("Pattern matching failure in match expression at match_heuristic.ml[4:21 ..4:28]")
      end
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[4:21 ..4:28]")
    end
  end
  local function common_suffix(k)
    local ey = k._1
    local ex = k._2
    if ey == 1 then
      if ex == 2 then
        return "foo"
      elseif ex == 3 then
        return "bar"
      else
        return error("Pattern matching failure in match expression at match_heuristic.ml[8:21 ..8:28]")
      end
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[8:21 ..8:28]")
    end
  end
  local function mixed_1(l)
    local fb = l._2
    local ff, fe = fb._1, fb._2
    if l._1 then
      if ff == 1 then
        return 1
      else
        if fe.__tag == "Cons" then
          return 3
        else
          return error("Pattern matching failure in match expression at match_heuristic.ml[13:15 ..13:22]")
        end
      end
    else
      if fe.__tag == "Nil" then
        if ff == 2 then
          return 2
        else
          return error("Pattern matching failure in match expression at match_heuristic.ml[13:15 ..13:22]")
        end
      elseif fe.__tag == "Cons" then
        return 3
      end
    end
  end
  local bottom = nil
  bottom({ common_prefix = common_prefix, common_suffix = common_suffix, mixed_1 = mixed_1 })
end
