do
  local function Stream(a) return { __tag = "Stream", a } end
  local function Some(a) return { __tag = "Some", a } end
  local None = { __tag = "None" }
  local Done = { __tag = "Done" }
  local function Yield(a) return { __tag = "Yield", a } end
  local function Skip(a) return { __tag = "Skip", a } end
  local print = print
  local to_string = tostring
  local function zip(tmp)
    local tmp0 = tmp[1]
    local f, start = tmp0._1, tmp0._2
    return function(tmp1)
      local tmp2 = tmp1[1]
      local g = tmp2._1
      return Stream({
        _1 = function(tmp3)
          local tmp4 = tmp3._2
          local sb = tmp4._1
          local x = tmp4._2
          local sa = tmp3._1
          if x.__tag == "None" then
            local tmp5 = f(sa)
            if tmp5.__tag == "Skip" then
              return Skip({ _2 = { _1 = sb, _2 = None }, _1 = tmp5[1] })
            elseif tmp5.__tag == "Yield" then
              local tmp6 = tmp5[1]
              return Skip({ _2 = { _1 = sb, _2 = Some(tmp6._1) }, _1 = tmp6._2 })
            elseif tmp5.__tag == "Done" then
              return Done
            end
          else
            local tmp5 = g(sb)
            local x0 = x[1]
            if tmp5.__tag == "Skip" then
              return Skip({ _1 = sa, _2 = { _1 = tmp5[1], _2 = Some(x0) } })
            elseif tmp5.__tag == "Yield" then
              local tmp6 = tmp5[1]
              return Yield({
                _1 = { _1 = x0, _2 = tmp6._1 },
                _2 = { _1 = sa, _2 = { _1 = tmp6._2, _2 = None } }
              })
            elseif tmp5.__tag == "Done" then
              return Done
            end
          end
        end,
        _2 = { _1 = start, _2 = { _1 = tmp2._2, _2 = None } }
      })
    end
  end
  local tmp = zip(Stream({
    _1 = function(n)
      if n > 100 then return Done end
      return Yield({ _1 = n, _2 = n + 1 })
    end,
    _2 = 1
  }))(Stream({
    _1 = function(n)
      if n > 300 then return Done end
      return Yield({ _1 = n, _2 = n + 1 })
    end,
    _2 = 100
  }))[1]
  local go = tmp._1
  local function go0(ac, st)
    local tmp0 = go(st)
    if tmp0.__tag == "Skip" then
      return go0(ac, tmp0[1])
    elseif tmp0.__tag == "Yield" then
      local tmp1 = tmp0[1]
      local x = tmp1._1
      return go0(x._1 + x._2 + ac, tmp1._2)
    elseif tmp0.__tag == "Done" then
      return ac
    end
  end
  print(to_string(go0(0, tmp._2)))
end
