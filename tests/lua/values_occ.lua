do
  local function Stream(a) return { __tag = "Stream", a } end
  local print = print
  local to_string = tostring
  local function sum_squares(xs)
    local js = xs[1]
    local go = js._1
    return Stream({
      _1 = function(st)
        local kk = go(st)
        local x = kk._1
        return { _2 = kk._2, _1 = x * x }
      end,
      _2 = js._2
    })
  end
  print(to_string(sum_squares))
end
