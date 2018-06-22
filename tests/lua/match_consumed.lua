do
  local __builtin_unit = {
    __tag = "__builtin_unit"
  }
  local function _plus (l)
    return function (r)
      return l + r
    end
  end
  local None = {
    __tag = "None"
  }
  local function Some (x)
    return {
      __tag = "Some",
      [1] = x
    }
  end
  local function main (f)
    return function (x)
      local a = f(1)
      if x.__tag == "None" then
        return f(a)
      elseif x.__tag == "Some" then
        local cw = x[1] * 2
        local ct = _plus(a)
        return f(ct(cw))
      end
    end
  end
  main()()
end
