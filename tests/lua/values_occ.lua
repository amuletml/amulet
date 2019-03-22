do
  local function Stream(x) return { __tag = "Stream", x } end
  local print = print
  local to_string = tostring
  local function sum_squares(xs)
    local iz = xs[1]
    local go = iz._1
    return Stream({
      _2 = iz._2,
      _1 = function(st)
        local js = go(st)
        local x = js._1
        return { _2 = js._2, _1 = x * x }
      end
    })
  end
  print(to_string(sum_squares))
end
