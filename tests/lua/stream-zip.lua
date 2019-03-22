do
  local None = { __tag = "None" }
  local function Some(x) return { __tag = "Some", x } end
  local Done = { __tag = "Done" }
  local function Yield(x) return { __tag = "Yield", x } end
  local function Skip(x) return { __tag = "Skip", x } end
  local function Stream(x) return { __tag = "Stream", x } end
  local print = print
  local to_string = tostring
  local function zip(bdf)
    local bdh = bdf[1]
    local f, start = bdh._1, bdh._2
    return function(bcz)
      local bdb = bcz[1]
      local g = bdb._1
      return Stream({
        _1 = function(bcl)
          local bcn = bcl._2
          local sb = bcn._1
          local x = bcn._2
          local sa = bcl._1
          if x.__tag == "Some" then
            local x0 = x[1]
            local bbo = g(sb)
            if bbo.__tag == "Skip" then
              return Skip({ _1 = sa, _2 = { _1 = bbo[1], _2 = Some(x0) } })
            elseif bbo.__tag == "Yield" then
              local bcf = bbo[1]
              return Yield({
                _2 = { _1 = sa, _2 = { _2 = None, _1 = bcf._2 } },
                _1 = { _1 = x0, _2 = bcf._1 }
              })
            elseif bbo.__tag == "Done" then
              return Done
            end
          elseif x.__tag == "None" then
            local bav = f(sa)
            if bav.__tag == "Skip" then
              return Skip({ _1 = bav[1], _2 = { _1 = sb, _2 = None } })
            elseif bav.__tag == "Yield" then
              local bbk = bav[1]
              return Skip({ _2 = { _1 = sb, _2 = Some(bbk._1) }, _1 = bbk._2 })
            elseif bav.__tag == "Done" then
              return Done
            end
          end
        end,
        _2 = { _1 = start, _2 = { _1 = bdb._2, _2 = None } }
      })
    end
  end
  local axo = zip(Stream({
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
  local go = axo._1
  local function go0(ac, st)
    local awv = go(st)
    if awv.__tag == "Skip" then
      return go0(ac, awv[1])
    elseif awv.__tag == "Yield" then
      local axf = awv[1]
      local x = axf._1
      return go0(x._1 + x._2 + ac, axf._2)
    elseif awv.__tag == "Done" then
      return ac
    end
  end
  print(to_string(go0(0, axo._2)))
end
