do
  local None = { __tag = "None" }
  local function Some(x) return { __tag = "Some", x } end
  local Done = { __tag = "Done" }
  local function Yield(x) return { __tag = "Yield", x } end
  local function Skip(x) return { __tag = "Skip", x } end
  local function Stream(x) return { __tag = "Stream", x } end
  local print = print
  local to_string = tostring
  local function zip(bdk)
    local bdm = bdk[1]
    local f, start = bdm._1, bdm._2
    return function(bde)
      local bdg = bde[1]
      local g = bdg._1
      return Stream({
        _2 = { _1 = start, _2 = { _2 = None, _1 = bdg._2 } },
        _1 = function(bcq)
          local bcs = bcq._2
          local x = bcs._2
          local sa = bcq._1
          local sb = bcs._1
          if x.__tag == "Some" then
            local x0 = x[1]
            local bbt = g(sb)
            if bbt.__tag == "Skip" then
              return Skip({ _1 = sa, _2 = { _2 = Some(x0), _1 = bbt[1] } })
            elseif bbt.__tag == "Yield" then
              local bck = bbt[1]
              return Yield({
                _1 = { _1 = x0, _2 = bck._1 },
                _2 = { _1 = sa, _2 = { _1 = bck._2, _2 = None } }
              })
            elseif bbt.__tag == "Done" then
              return Done
            end
          elseif x.__tag == "None" then
            local bba = f(sa)
            if bba.__tag == "Skip" then
              return Skip({ _2 = { _1 = sb, _2 = None }, _1 = bba[1] })
            elseif bba.__tag == "Yield" then
              local bbp = bba[1]
              return Skip({ _2 = { _1 = sb, _2 = Some(bbp._1) }, _1 = bbp._2 })
            elseif bba.__tag == "Done" then
              return Done
            end
          end
        end
      })
    end
  end
  local axt = zip(Stream({
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
  local go = axt._1
  local function go0(ac, st)
    local axa = go(st)
    if axa.__tag == "Skip" then
      return go0(ac, axa[1])
    elseif axa.__tag == "Yield" then
      local axk = axa[1]
      local x = axk._1
      return go0(x._1 + x._2 + ac, axk._2)
    elseif axa.__tag == "Done" then
      return ac
    end
  end
  print(to_string(go0(0, axt._2)))
end
