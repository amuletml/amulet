do
  local None = { __tag = "None" }
  local function Some(x) return { __tag = "Some", x } end
  local Done = { __tag = "Done" }
  local function Yield(x) return { __tag = "Yield", x } end
  local function Skip(x) return { __tag = "Skip", x } end
  local function Stream(x) return { __tag = "Stream", x } end
  local print = print
  local to_string = tostring
  local function zip(bga)
    local bgc = bga[1]
    local f, start = bgc._1, bgc._2
    return function(bfv)
      local bfx = bfv[1]
      local g = bfx._1
      return Stream({
        _2 = { _1 = start, _2 = { _2 = None, _1 = bfx._2 } },
        _1 = function(bfj)
          local bfl = bfj._2
          local sb = bfl._1
          local x = bfl._2
          local sa = bfj._1
          if x.__tag == "Some" then
            local ben = g(sb)
            local x0 = x[1]
            if ben.__tag == "Skip" then
              return Skip({ _1 = sa, _2 = { _1 = ben[1], _2 = Some(x0) } })
            elseif ben.__tag == "Yield" then
              local bfe = ben[1]
              return Yield({
                _1 = { _1 = x0, _2 = bfe._1 },
                _2 = { _1 = sa, _2 = { _2 = None, _1 = bfe._2 } }
              })
            elseif ben.__tag == "Done" then
              return Done
            end
          else
            local bdv = f(sa)
            if bdv.__tag == "Skip" then
              return Skip({ _1 = bdv[1], _2 = { _1 = sb, _2 = None } })
            elseif bdv.__tag == "Yield" then
              local bek = bdv[1]
              return Skip({ _2 = { _1 = sb, _2 = Some(bek._1) }, _1 = bek._2 })
            elseif bdv.__tag == "Done" then
              return Done
            end
          end
        end
      })
    end
  end
  local baw = zip(Stream({
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
  local go = baw._1
  local function go0(ac, st)
    local bae = go(st)
    if bae.__tag == "Skip" then
      return go0(ac, bae[1])
    elseif bae.__tag == "Yield" then
      local bao = bae[1]
      local x = bao._1
      return go0(x._1 + x._2 + ac, bao._2)
    elseif bae.__tag == "Done" then
      return ac
    end
  end
  print(to_string(go0(0, baw._2)))
end
