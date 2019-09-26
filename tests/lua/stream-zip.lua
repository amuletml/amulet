do
  local None = { __tag = "None" }
  local function Some(a) return { __tag = "Some", a } end
  local Done = { __tag = "Done" }
  local function Yield(a) return { __tag = "Yield", a } end
  local function Skip(a) return { __tag = "Skip", a } end
  local function Stream(a) return { __tag = "Stream", a } end
  local print = print
  local to_string = tostring
  local function zip(bfz)
    local bgb = bfz[1]
    local f, start = bgb._1, bgb._2
    return function(bfu)
      local bfw = bfu[1]
      local g = bfw._1
      return Stream({
        _2 = { _1 = start, _2 = { _2 = None, _1 = bfw._2 } },
        _1 = function(bfi)
          local bfk = bfi._2
          local sb = bfk._1
          local x = bfk._2
          local sa = bfi._1
          if x.__tag == "Some" then
            local bem = g(sb)
            local x0 = x[1]
            if bem.__tag == "Skip" then
              return Skip({ _1 = sa, _2 = { _1 = bem[1], _2 = Some(x0) } })
            elseif bem.__tag == "Yield" then
              local bfd = bem[1]
              return Yield({
                _2 = { _1 = sa, _2 = { _2 = None, _1 = bfd._2 } },
                _1 = { _1 = x0, _2 = bfd._1 }
              })
            elseif bem.__tag == "Done" then
              return Done
            end
          else
            local bdu = f(sa)
            if bdu.__tag == "Skip" then
              return Skip({ _1 = bdu[1], _2 = { _1 = sb, _2 = None } })
            elseif bdu.__tag == "Yield" then
              local bej = bdu[1]
              return Skip({ _1 = bej._2, _2 = { _1 = sb, _2 = Some(bej._1) } })
            elseif bdu.__tag == "Done" then
              return Done
            end
          end
        end
      })
    end
  end
  local bav = zip(Stream({
    _1 = function(n)
      if n > 100 then
        return Done
      end
      return Yield({ _1 = n, _2 = n + 1 })
    end,
    _2 = 1
  }))(Stream({
    _1 = function(n)
      if n > 300 then
        return Done
      end
      return Yield({ _1 = n, _2 = n + 1 })
    end,
    _2 = 100
  }))[1]
  local go = bav._1
  local function go0(ac, st)
    local bad = go(st)
    if bad.__tag == "Skip" then
      return go0(ac, bad[1])
    elseif bad.__tag == "Yield" then
      local ban = bad[1]
      local x = ban._1
      return go0(x._1 + x._2 + ac, ban._2)
    elseif bad.__tag == "Done" then
      return ac
    end
  end
  print(to_string(go0(0, bav._2)))
end
