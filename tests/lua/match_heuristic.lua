do
  local function common_prefix(f)
    local _do = f._1
    local dn = f._2
    if _do ~= 1 then
      return error("Pattern matching failure in match expression at match_heuristic.ml[2:21 ..2:28]")
    end
    if dn == 2 then
      return "foo"
    elseif dn == 3 then
      return "bar"
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[2:21 ..2:28]")
    end
  end
  local function common_suffix(g)
    local dr = g._1
    local dq = g._2
    if dr ~= 1 then
      return error("Pattern matching failure in match expression at match_heuristic.ml[6:21 ..6:28]")
    end
    if dq == 2 then
      return "foo"
    elseif dq == 3 then
      return "bar"
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[6:21 ..6:28]")
    end
  end
  local function mixed_1(h)
    local dt = h._2
    local dw, dv = dt._1, dt._2
    if h._1 then
      if dw == 1 then
        return 1
      end
      if dv.__tag == "Cons" then
        return 3
      end
      return error("Pattern matching failure in match expression at match_heuristic.ml[11:15 ..11:22]")
    else
      if dv.__tag ~= "Nil" then
        return 3
      end
      if dw == 2 then
        return 2
      end
      return error("Pattern matching failure in match expression at match_heuristic.ml[11:15 ..11:22]")
    end
  end
  (nil)({ common_prefix = common_prefix, common_suffix = common_suffix, mixed_1 = mixed_1 })
end
