do
  local None = { __tag = "None" }
  local function Some(a) return { __tag = "Some", a } end
  local Done = { __tag = "Done" }
  local function Yield(a) return { __tag = "Yield", a } end
  local function Skip(a) return { __tag = "Skip", a } end
  local function Stream(a) return { __tag = "Stream", a } end
  local print = print
  local to_string = tostring
  local function zip(bgv)
    local bgx = bgv[1]
    local f, start = bgx._1, bgx._2
    return function(bgq)
      local bgs = bgq[1]
      local g = bgs._1
      return Stream({
        _2 = { _1 = start, _2 = { _1 = bgs._2, _2 = None } },
        _1 = function(bgc)
          local sa = bgc._1
          local bge = bgc._2
          local sb = bge._1
          local x = bge._2
          if x.__tag == "Some" then
            local bez = g(sb)
            local x0 = x[1]
            if bez.__tag == "Skip" then
              return Skip({ _1 = sa, _2 = { _1 = bez[1], _2 = Some(x0) } })
            elseif bez.__tag == "Yield" then
              local bfx = bez[1]
              return Yield({
                _1 = { _1 = x0, _2 = bfx._1 },
                _2 = { _1 = sa, _2 = { _2 = None, _1 = bfx._2 } }
              })
            elseif bez.__tag == "Done" then
              return Done
            end
          else
            local beb = f(sa)
            if beb.__tag == "Skip" then
              return Skip({ _2 = { _1 = sb, _2 = None }, _1 = beb[1] })
            elseif beb.__tag == "Yield" then
              local bew = beb[1]
              return Skip({ _1 = bew._2, _2 = { _1 = sb, _2 = Some(bew._1) } })
            elseif beb.__tag == "Done" then
              return Done
            end
          end
        end
      })
    end
  end
  local bal = zip(Stream({
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
  local go = bal._1
  local function go0(ac, st)
    local bam = go(st)
    if bam.__tag == "Skip" then
      return go0(ac, bam[1])
    elseif bam.__tag == "Yield" then
      local bab = bam[1]
      local x = bab._1
      return go0(x._1 + x._2 + ac, bab._2)
    elseif bam.__tag == "Done" then
      return ac
    end
  end
  print(to_string(go0(0, bal._2)))
end
