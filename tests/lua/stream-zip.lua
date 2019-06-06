do
  local None = { __tag = "None" }
  local function Some(x) return { __tag = "Some", x } end
  local Done = { __tag = "Done" }
  local function Yield(x) return { __tag = "Yield", x } end
  local function Skip(x) return { __tag = "Skip", x } end
  local function Stream(x) return { __tag = "Stream", x } end
  local print = print
  local to_string = tostring
  local function zip(bco)
    local bcq = bco[1]
    local f, start = bcq._1, bcq._2
    return function(bcj)
      local bcl = bcj[1]
      local g = bcl._1
      return Stream({
        _1 = function(bbx)
          local bbz = bbx._2
          local x = bbz._2
          local sa = bbx._1
          local sb = bbz._1
          if x.__tag == "Some" then
            local x0 = x[1]
            local bbb = g(sb)
            if bbb.__tag == "Skip" then
              return Skip({ _1 = sa, _2 = { _2 = Some(x0), _1 = bbb[1] } })
            elseif bbb.__tag == "Yield" then
              local bbs = bbb[1]
              return Yield({
                _1 = { _1 = x0, _2 = bbs._1 },
                _2 = { _1 = sa, _2 = { _2 = None, _1 = bbs._2 } }
              })
            elseif bbb.__tag == "Done" then
              return Done
            end
          elseif x.__tag == "None" then
            local baj = f(sa)
            if baj.__tag == "Skip" then
              return Skip({ _2 = { _1 = sb, _2 = None }, _1 = baj[1] })
            elseif baj.__tag == "Yield" then
              local bay = baj[1]
              return Skip({ _2 = { _1 = sb, _2 = Some(bay._1) }, _1 = bay._2 })
            elseif baj.__tag == "Done" then
              return Done
            end
          end
        end,
        _2 = { _1 = start, _2 = { _1 = bcl._2, _2 = None } }
      })
    end
  end
  local axk = zip(Stream({
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
  local go = axk._1
  local function go0(ac, st)
    local aws = go(st)
    if aws.__tag == "Skip" then
      return go0(ac, aws[1])
    elseif aws.__tag == "Yield" then
      local axc = aws[1]
      local x = axc._1
      return go0(x._1 + x._2 + ac, axc._2)
    elseif aws.__tag == "Done" then
      return ac
    end
  end
  print(to_string(go0(0, axk._2)))
end
