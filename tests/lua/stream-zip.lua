do
  local None = { __tag = "None" }
  local function Some(x) return { __tag = "Some", x } end
  local Done = { __tag = "Done" }
  local function Yield(x) return { __tag = "Yield", x } end
  local function Skip(x) return { __tag = "Skip", x } end
  local function Stream(x) return { __tag = "Stream", x } end
  local print = print
  local to_string = tostring
  local function zip(bda)
    local bdc = bda[1]
    local f, start = bdc._1, bdc._2
    return function(bcv)
      local bcx = bcv[1]
      local g = bcx._1
      return Stream({
        _2 = { _1 = start, _2 = { _2 = None, _1 = bcx._2 } },
        _1 = function(bcj)
          local bcl = bcj._2
          local x = bcl._2
          local sa = bcj._1
          local sb = bcl._1
          if x.__tag == "Some" then
            local bbn = g(sb)
            local x0 = x[1]
            if bbn.__tag == "Skip" then
              return Skip({ _1 = sa, _2 = { _1 = bbn[1], _2 = Some(x0) } })
            elseif bbn.__tag == "Yield" then
              local bce = bbn[1]
              return Yield({
                _1 = { _1 = x0, _2 = bce._1 },
                _2 = { _1 = sa, _2 = { _2 = None, _1 = bce._2 } }
              })
            elseif bbn.__tag == "Done" then
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
        end
      })
    end
  end
  local axu = zip(Stream({
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
  local go = axu._1
  local function go0(ac, st)
    local axc = go(st)
    if axc.__tag == "Skip" then
      return go0(ac, axc[1])
    elseif axc.__tag == "Yield" then
      local axm = axc[1]
      local x = axm._1
      return go0(x._1 + x._2 + ac, axm._2)
    elseif axc.__tag == "Done" then
      return ac
    end
  end
  print(to_string(go0(0, axu._2)))
end
