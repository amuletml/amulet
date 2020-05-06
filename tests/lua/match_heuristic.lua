do
  local function common_prefix(x)
    local tmp = x._1
    local tmp0 = x._2
    if tmp ~= 1 then
      return error("Pattern matching failure in match expression at match_heuristic.ml[2:21 ..4:17]")
    end
    if tmp0 == 2 then
      return "foo"
    elseif tmp0 == 3 then
      return "bar"
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[2:21 ..4:17]")
    end
  end
  local function common_suffix(x)
    local tmp = x._1
    local tmp0 = x._2
    if tmp ~= 1 then
      return error("Pattern matching failure in match expression at match_heuristic.ml[6:21 ..8:17]")
    end
    if tmp0 == 2 then
      return "foo"
    elseif tmp0 == 3 then
      return "bar"
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[6:21 ..8:17]")
    end
  end
  local function mixed_1(x)
    local tmp = x._2
    local tmp0, tmp1 = tmp._1, tmp._2
    if x._1 then
      if tmp0 == 1 then return 1 end
      if tmp1.__tag == "Cons" then return 3 end
      return error("Pattern matching failure in match expression at match_heuristic.ml[11:15 ..14:21]")
    else
      if tmp1.__tag == "Cons" then return 3 end
      if tmp0 == 2 then return 2 end
      return error("Pattern matching failure in match expression at match_heuristic.ml[11:15 ..14:21]")
    end
  end
  (nil)({ common_prefix = common_prefix, common_suffix = common_suffix, mixed_1 = mixed_1 })
end
