do
  local print = print
  local to_string = tostring
  local function Stream(x) return { __tag = "Stream", x } end
  local function sum_squares(xs)
    local iw = xs[1]
    local go = iw._1
    return Stream({
      _1 = function(st)
        local jp = go(st)
        local x = jp._1
        return { _1 = x * x, _2 = jp._2 }
      end,
      _2 = iw._2
    })
  end
  print(to_string(sum_squares))
end
