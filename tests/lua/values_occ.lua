do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local print = print
  local to_string = tostring
  local function Yield(x) return { __tag = "Yield", x } end
  local function Stream(x) return { __tag = "Stream", x } end
  local function sum_squares(xs)
    local go = xs[1]._1
    return Stream({
      _1 = function(st)
        local hr = go(st)
        local x = hr[1]._1
        return Yield({ _1 = x * x, _2 = hr[1]._2 })
      end,
      _2 = xs[1]._2
    })
  end
  print(to_string(sum_squares))
end
