do
  local function Stream(a) return { __tag = "Stream", a } end
  local print = print
  local to_string = tostring
  local function sum_squares(xs)
    local tmp = xs[1]
    local go = tmp._1
    return Stream({
      _1 = function(st)
        local tmp0 = go(st)
        local x = tmp0._1
        return { _1 = x * x, _2 = tmp0._2 }
      end,
      _2 = tmp._2
    })
  end
  print(to_string(sum_squares))
end
