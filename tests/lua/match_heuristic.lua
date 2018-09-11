do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function common_prefix(j)
    local ef = j._1
    local ee = j._2
    if 1 == ef then
      if 2 == ee then
        return "foo"
      elseif 3 == ee then
        return "bar"
      else
        return error("Pattern matching failure in match expression at match_heuristic.ml[4:21 ..4:28]")
      end
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[4:21 ..4:28]")
    end
  end
  local function common_suffix(k)
    local el = k._1
    local ek = k._2
    if 1 == el then
      if 2 == ek then
        return "foo"
      elseif 3 == ek then
        return "bar"
      else
        return error("Pattern matching failure in match expression at match_heuristic.ml[8:21 ..8:28]")
      end
    else
      return error("Pattern matching failure in match expression at match_heuristic.ml[8:21 ..8:28]")
    end
  end
  local bottom = nil
  bottom({
    common_prefix = common_prefix,
    common_suffix = common_suffix,
    mixed_1 = function(l)
      local er = l._1
      local eq = l._2
      local eu = eq._1
      local et = eq._2
      if 1 == eu then
        if true == er then
          return 1
        else
          if et.__tag == "Cons" then
            return 3
          else
            return error("Pattern matching failure in match expression at match_heuristic.ml[13:15 ..13:22]")
          end
        end
      elseif 2 == eu then
        if et.__tag == "Nil" then
          if false == er then
            return 2
          else
            return error("Pattern matching failure in match expression at match_heuristic.ml[13:15 ..13:22]")
          end
        elseif et.__tag == "Cons" then
          return 3
        end
      else
        if et.__tag == "Cons" then
          return 3
        else
          return error("Pattern matching failure in match expression at match_heuristic.ml[13:15 ..13:22]")
        end
      end
    end
  })
end
