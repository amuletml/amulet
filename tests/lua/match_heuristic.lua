do
  local function common_prefix(f)
    local dj = f._1
    local di = f._2
    if dj == 1 then
      if di == 2 then
        return "foo"
      elseif di == 3 then
        return "bar"
      else
        return error("Pattern matching failure in match expression at match_heuristic.ml[2:21 ..2:28]")
      end
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[2:21 ..2:28]")
    end
  end
  local function common_suffix(g)
    local dn = g._1
    local dm = g._2
    if dn == 1 then
      if dm == 2 then
        return "foo"
      elseif dm == 3 then
        return "bar"
      else
        return error("Pattern matching failure in match expression at match_heuristic.ml[6:21 ..6:28]")
      end
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[6:21 ..6:28]")
    end
  end
  local function mixed_1(h)
    local dq = h._2
    local du, dt = dq._1, dq._2
    if h._1 then
      if du == 1 then
        return 1
      else
        if dt.__tag == "Cons" then
          return 3
        else
          return error("Pattern matching failure in match expression at match_heuristic.ml[11:15 ..11:22]")
        end
      end
    else
      if dt.__tag == "Nil" then
        if du == 2 then
          return 2
        else
          return error("Pattern matching failure in match expression at match_heuristic.ml[11:15 ..11:22]")
        end
      elseif dt.__tag == "Cons" then
        return 3
      end
    end
  end
  (nil)({ common_prefix = common_prefix, common_suffix = common_suffix, mixed_1 = mixed_1 })
end
