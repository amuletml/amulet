do
  local function Stream(a) return { __tag = "Stream", a } end
  local print = print
  local to_string = tostring
  local function sum_squares(xs)
    local jq = xs[1]
    local go = jq._1
    return Stream({
      _1 = function(st)
        local kk = go(st)
        local x = kk._1
        return { _2 = kk._2, _1 = x * x }
      end,
      _2 = jq._2
    })
  end
  print(to_string(sum_squares))
end
