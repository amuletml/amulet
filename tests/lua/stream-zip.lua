do
  local print = print
  local to_string = tostring
  local function Skip(x) return { __tag = "Skip", x } end
  local function Yield(x) return { __tag = "Yield", x } end
  local Done = { __tag = "Done" }
  local function Stream(x) return { __tag = "Stream", x } end
  local function Some(x) return { __tag = "Some", x } end
  local None = { __tag = "None" }
  local function zip(bfv)
    local bfx = bfv[1]
    local f, start = bfx._1, bfx._2
    return function(bfp)
      local bfr = bfp[1]
      local g = bfr._1
      return Stream({
        _1 = function(bfb)
          local bfd = bfb._2
          local sb = bfd._1
          local x = bfd._2
          local sa = bfb._1
          if x.__tag == "Some" then
            local x0 = x[1]
            local bee = g(sb)
            if bee.__tag == "Skip" then
              return Skip({ _1 = sa, _2 = { _1 = bee[1], _2 = Some(x0) } })
            elseif bee.__tag == "Yield" then
              local bev = bee[1]
              return Yield({
                _1 = { _1 = x0, _2 = bev._1 },
                _2 = { _1 = sa, _2 = { _1 = bev._2, _2 = None } }
              })
            elseif bee.__tag == "Done" then
              return Done
            end
          elseif x.__tag == "None" then
            local bdl = f(sa)
            if bdl.__tag == "Skip" then
              return Skip({ _1 = bdl[1], _2 = { _1 = sb, _2 = None } })
            elseif bdl.__tag == "Yield" then
              local bea = bdl[1]
              return Skip({ _1 = bea._2, _2 = { _1 = sb, _2 = Some(bea._1) } })
            elseif bdl.__tag == "Done" then
              return Done
            end
          end
        end,
        _2 = { _1 = start, _2 = { _1 = bfr._2, _2 = None } }
      })
    end
  end
  local bae = zip(Stream({
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
  local go = bae._1
  local function go0(ac, st)
    local bal = go(st)
    if bal.__tag == "Skip" then
      return go0(ac, bal[1])
    elseif bal.__tag == "Yield" then
      local bav = bal[1]
      local x = bav._1
      return go0(x._1 + x._2 + ac, bav._2)
    elseif bal.__tag == "Done" then
      return ac
    end
  end
  print(to_string(go0(0, bae._2)))
end
