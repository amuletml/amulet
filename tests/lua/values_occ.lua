do
  local function Stream(x) return { __tag = "Stream", x } end
  local print = print
  local to_string = tostring
  local function sum_squares(xs)
    local jt = xs[1]
    local go = jt._1
    return Stream({
      _1 = function(st)
        local kl = go(st)
        local x = kl._1
        return { _1 = x * x, _2 = kl._2 }
      end,
      _2 = jt._2
    })
  end
  print(to_string(sum_squares))
end
