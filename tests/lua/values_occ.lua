do
  local print = print
  local to_string = tostring
  local function Stream(x) return { __tag = "Stream", x } end
  local function sum_squares(xs)
    local jc = xs[1]
    local go = jc._1
    return Stream({
      _1 = function(st)
        local jv = go(st)
        local x = jv._1
        return { _1 = x * x, _2 = jv._2 }
      end,
      _2 = jc._2
    })
  end
  print(to_string(sum_squares))
end
