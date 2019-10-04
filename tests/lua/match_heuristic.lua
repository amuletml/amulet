do
  local function common_prefix(f)
    local dv = f._1
    local du = f._2
    if dv ~= 1 then
      return error("Pattern matching failure in match expression at match_heuristic.ml[2:21 ..2:28]")
    end
    if du == 2 then
      return "foo"
    elseif du == 3 then
      return "bar"
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[2:21 ..2:28]")
    end
  end
  local function common_suffix(g)
    local dy = g._1
    local dx = g._2
    if dy ~= 1 then
      return error("Pattern matching failure in match expression at match_heuristic.ml[6:21 ..6:28]")
    end
    if dx == 2 then
      return "foo"
    elseif dx == 3 then
      return "bar"
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[6:21 ..6:28]")
    end
  end
  local function mixed_1(h)
    local ea = h._2
    local ed, ec = ea._1, ea._2
    if h._1 then
      if ed == 1 then
        return 1
      end
      if ec.__tag == "Cons" then
        return 3
      end
      return error("Pattern matching failure in match expression at match_heuristic.ml[11:15 ..11:22]")
    else
      if ec.__tag ~= "Nil" then
        return 3
      end
      if ed == 2 then
        return 2
      end
      return error("Pattern matching failure in match expression at match_heuristic.ml[11:15 ..11:22]")
    end
  end
  (nil)({ common_prefix = common_prefix, common_suffix = common_suffix, mixed_1 = mixed_1 })
end
