do
  local function Stream(x) return { __tag = "Stream", x } end
  local print = print
  local to_string = tostring
  local function sum_squares(xs)
    local jj = xs[1]
    local go = jj._1
    return Stream({
      _2 = jj._2,
      _1 = function(st)
        local kc = go(st)
        local x = kc._1
        return { _1 = x * x, _2 = kc._2 }
      end
    })
  end
  print(to_string(sum_squares))
end
