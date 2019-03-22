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
    local dl = g._2
    local dm = g._1
    if dm == 1 then
      if dl == 2 then
        return "foo"
      elseif dl == 3 then
        return "bar"
      else
        return error("Pattern matching failure in match expression at match_heuristic.ml[6:21 ..6:28]")
      end
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[6:21 ..6:28]")
    end
  end
  local function mixed_1(h)
    local dp = h._2
    local dt, ds = dp._1, dp._2
    if h._1 then
      if dt == 1 then
        return 1
      else
        if ds.__tag == "Cons" then
          return 3
        else
          return error("Pattern matching failure in match expression at match_heuristic.ml[11:15 ..11:22]")
        end
      end
    else
      if ds.__tag == "Nil" then
        if dt == 2 then
          return 2
        else
          return error("Pattern matching failure in match expression at match_heuristic.ml[11:15 ..11:22]")
        end
      elseif ds.__tag == "Cons" then
        return 3
      end
    end
  end
  (nil)({ common_prefix = common_prefix, common_suffix = common_suffix, mixed_1 = mixed_1 })
end
