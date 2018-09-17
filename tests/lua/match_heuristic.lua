do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function common_prefix(j)
    local er = j._2
    local es = j._1
    if 1 == es then
      if 2 == er then
        return "foo"
      elseif 3 == er then
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
    if 1 == ey then
      if 2 == ex then
        return "foo"
      elseif 3 == ex then
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
      local fd = l._2
      local fh = fd._1
      local fg = fd._2
      if l._1 then
        if 1 == fh then
          return 1
        else
          if fg.__tag == "Cons" then
            return 3
          else
            return error("Pattern matching failure in match expression at match_heuristic.ml[13:15 ..13:22]")
          end
        end
      else
        if fg.__tag == "Nil" then
          if 2 == fh then
            return 2
          else
            return error("Pattern matching failure in match expression at match_heuristic.ml[13:15 ..13:22]")
          end
        elseif fg.__tag == "Cons" then
          return 3
        end
      end
    end
  })
end
