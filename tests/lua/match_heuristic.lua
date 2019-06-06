do
  local function common_prefix(f)
    local di = f._1
    local dh = f._2
    if di == 1 then
      if dh == 2 then
        return "foo"
      elseif dh == 3 then
        return "bar"
      else
        return error("Pattern matching failure in match expression at match_heuristic.ml[2:21 ..2:28]")
      end
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[2:21 ..2:28]")
    end
  end
  local function common_suffix(g)
    local dl = g._1
    local dk = g._2
    if dl == 1 then
      if dk == 2 then
        return "foo"
      elseif dk == 3 then
        return "bar"
      else
        return error("Pattern matching failure in match expression at match_heuristic.ml[6:21 ..6:28]")
      end
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[6:21 ..6:28]")
    end
  end
  local function mixed_1(h)
    local dn = h._2
    local dq, dp = dn._1, dn._2
    if h._1 then
      if dq == 1 then
        return 1
      else
        if dp.__tag == "Cons" then
          return 3
        else
          return error("Pattern matching failure in match expression at match_heuristic.ml[11:15 ..11:22]")
        end
      end
    else
      if dp.__tag == "Nil" then
        if dq == 2 then
          return 2
        else
          return error("Pattern matching failure in match expression at match_heuristic.ml[11:15 ..11:22]")
        end
      elseif dp.__tag == "Cons" then
        return 3
      end
    end
  end
  (nil)({ common_prefix = common_prefix, common_suffix = common_suffix, mixed_1 = mixed_1 })
end
