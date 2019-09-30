do
  local None = { __tag = "None" }
  local function Some(a) return { __tag = "Some", a } end
  local Done = { __tag = "Done" }
  local function Yield(a) return { __tag = "Yield", a } end
  local function Skip(a) return { __tag = "Skip", a } end
  local function Stream(a) return { __tag = "Stream", a } end
  local print = print
  local to_string = tostring
  local function zip(bez)
    local bfb = bez[1]
    local f, start = bfb._1, bfb._2
    return function(beu)
      local bew = beu[1]
      local g = bew._1
      return Stream({
        _1 = function(bei)
          local bek = bei._2
          local sb = bek._1
          local x = bek._2
          local sa = bei._1
          if x.__tag == "Some" then
            local x0 = x[1]
            local bdm = g(sb)
            if bdm.__tag == "Skip" then
              return Skip({ _1 = sa, _2 = { _2 = Some(x0), _1 = bdm[1] } })
            elseif bdm.__tag == "Yield" then
              local bed = bdm[1]
              return Yield({
                _1 = { _1 = x0, _2 = bed._1 },
                _2 = { _1 = sa, _2 = { _2 = None, _1 = bed._2 } }
              })
            elseif bdm.__tag == "Done" then
              return Done
            end
          else
            local bcu = f(sa)
            if bcu.__tag == "Skip" then
              return Skip({ _1 = bcu[1], _2 = { _1 = sb, _2 = None } })
            elseif bcu.__tag == "Yield" then
              local bdj = bcu[1]
              return Skip({ _2 = { _1 = sb, _2 = Some(bdj._1) }, _1 = bdj._2 })
            elseif bcu.__tag == "Done" then
              return Done
            end
          end
        end,
        _2 = { _1 = start, _2 = { _1 = bew._2, _2 = None } }
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
