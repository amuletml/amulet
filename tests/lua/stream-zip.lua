do
  local None = { __tag = "None" }
  local function Some(x) return { __tag = "Some", x } end
  local Done = { __tag = "Done" }
  local function Yield(x) return { __tag = "Yield", x } end
  local function Skip(x) return { __tag = "Skip", x } end
  local function Stream(x) return { __tag = "Stream", x } end
  local print = print
  local to_string = tostring
  local function zip(bcq)
    local bcs = bcq[1]
    local f, start = bcs._1, bcs._2
    return function(bcl)
      local bcn = bcl[1]
      local g = bcn._1
      return Stream({
        _2 = { _1 = start, _2 = { _1 = bcn._2, _2 = None } },
        _1 = function(bbz)
          local bcb = bbz._2
          local sb = bcb._1
          local x = bcb._2
          local sa = bbz._1
          if x.__tag == "Some" then
            local x0 = x[1]
            local bbd = g(sb)
            if bbd.__tag == "Skip" then
              return Skip({ _1 = sa, _2 = { _2 = Some(x0), _1 = bbd[1] } })
            elseif bbd.__tag == "Yield" then
              local bbu = bbd[1]
              return Yield({
                _1 = { _1 = x0, _2 = bbu._1 },
                _2 = { _1 = sa, _2 = { _1 = bbu._2, _2 = None } }
              })
            elseif bbd.__tag == "Done" then
              return Done
            end
          elseif x.__tag == "None" then
            local bal = f(sa)
            if bal.__tag == "Skip" then
              return Skip({ _2 = { _1 = sb, _2 = None }, _1 = bal[1] })
            elseif bal.__tag == "Yield" then
              local bba = bal[1]
              return Skip({ _2 = { _1 = sb, _2 = Some(bba._1) }, _1 = bba._2 })
            elseif bal.__tag == "Done" then
              return Done
            end
          end
        end
      })
    end
  end
  local axm = zip(Stream({
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
  local go = axm._1
  local function go0(ac, st)
    local awu = go(st)
    if awu.__tag == "Skip" then
      return go0(ac, awu[1])
    elseif awu.__tag == "Yield" then
      local axe = awu[1]
      local x = axe._1
      return go0(x._1 + x._2 + ac, axe._2)
    elseif awu.__tag == "Done" then
      return ac
    end
  end
  print(to_string(go0(0, axm._2)))
end
