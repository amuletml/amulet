do
  local function common_prefix(f)
    local dn = f._1
    local dm = f._2
    if dn ~= 1 then
      return error("Pattern matching failure in match expression at match_heuristic.ml[2:21 ..2:28]")
    end
    if dm == 2 then
      return "foo"
    elseif dm == 3 then
      return "bar"
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[2:21 ..2:28]")
    end
  end
  local function common_suffix(g)
    local dp = g._2
    local dq = g._1
    if dq ~= 1 then
      return error("Pattern matching failure in match expression at match_heuristic.ml[6:21 ..6:28]")
    end
    if dp == 2 then
      return "foo"
    elseif dp == 3 then
      return "bar"
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[6:21 ..6:28]")
    end
  end
  local function mixed_1(h)
    local ds = h._2
    local dv, du = ds._1, ds._2
    if h._1 then
      if dv == 1 then
        return 1
      end
      if du.__tag == "Cons" then
        return 3
      end
      return error("Pattern matching failure in match expression at match_heuristic.ml[11:15 ..11:22]")
    else
      if du.__tag ~= "Nil" then
        return 3
      end
      if dv == 2 then
        return 2
      end
      return error("Pattern matching failure in match expression at match_heuristic.ml[11:15 ..11:22]")
    end
  end
  (nil)({ common_prefix = common_prefix, common_suffix = common_suffix, mixed_1 = mixed_1 })
end
