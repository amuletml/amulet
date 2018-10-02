do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local print = print
  local to_string = tostring
  local function Skip(x) return { __tag = "Skip", x } end
  local function Yield(x) return { __tag = "Yield", x } end
  local Done = { __tag = "Done" }
  local function Stream(x) return { __tag = "Stream", x } end
  local function Some(x) return { __tag = "Some", x } end
  local None = { __tag = "None" }
  local function zip(bat)
    local bav = bat[1]
    local f = bav._1
    local start = bav._2
    return function(ban)
      local bap = ban[1]
      local g = bap._1
      return Stream({
        _1 = function(baz)
          local bab = baz._2
          local sb = bab._1
          local x = bab._2
          local sa = baz._1
          if x.__tag == "Some" then
            local x0 = x[1]
            local bac = g(sb)
            if bac.__tag == "Skip" then
              return Skip({ _1 = sa, _2 = { _1 = bac[1], _2 = Some(x0) } })
            elseif bac.__tag == "Yield" then
              local bat0 = bac[1]
              return Yield({
                _1 = { _1 = x0, _2 = bat0._1 },
                _2 = { _1 = sa, _2 = { _1 = bat0._2, _2 = None } }
              })
            elseif bac.__tag == "Done" then
              return Done
            end
          elseif x.__tag == "None" then
            local ayj = f(sa)
            if ayj.__tag == "Skip" then
              return Skip({ _1 = ayj[1], _2 = { _1 = sb, _2 = None } })
            elseif ayj.__tag == "Yield" then
              local ayy = ayj[1]
              return Skip({ _1 = ayy._2, _2 = { _1 = sb, _2 = Some(ayy._1) } })
            elseif ayj.__tag == "Done" then
              return Done
            end
          end
        end,
        _2 = { _1 = start, _2 = { _1 = bap._2, _2 = None } }
      })
    end
  end
  local avc = zip(Stream({
    _1 = function(n)
      if n > 100 then
        return Done
      else
        return Yield({ _1 = n, _2 = n + 1 })
      end
    end,
    _2 = 1
  }))(Stream({
    _1 = function(n)
      if n > 300 then
        return Done
      else
        return Yield({ _1 = n, _2 = n + 1 })
      end
    end,
    _2 = 100
  }))[1]
  local go = avc._1
  local function go0(bmf_1, bmf_2)
    local auj = go(bmf_2)
    if auj.__tag == "Skip" then
      return go0(bmf_1, auj[1])
    elseif auj.__tag == "Yield" then
      local aut = auj[1]
      local x = aut._1
      return go0(x._1 + x._2 + bmf_1, aut._2)
    elseif auj.__tag == "Done" then
      return bmf_1
    end
  end
  print(to_string(go0(0, avc._2)))
end
