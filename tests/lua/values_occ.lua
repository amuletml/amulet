do
  local print = print
  local to_string = tostring
  local function Stream(x) return { __tag = "Stream", x } end
  local function sum_squares(xs)
    local _in = xs[1]
    local go = _in._1
    return Stream({
      _1 = function(st)
        local jg = go(st)
        local x = jg._1
        return { _1 = x * x, _2 = jg._2 }
      end,
      _2 = _in._2
    })
  end
  print(to_string(sum_squares))
end
