do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local print = print
  local to_string = tostring
  local function Stream(x) return { __tag = "Stream", x } end
  local function sum_squares(xs)
    local je = xs[1]
    local go = je._1
    return Stream({
      _1 = function(st)
        local jx = go(st)
        local x = jx._1
        return { _1 = x * x, _2 = jx._2 }
      end,
      _2 = je._2
    })
  end
  print(to_string(sum_squares))
end
