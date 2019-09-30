do
  local function Stream(a) return { __tag = "Stream", a } end
  local print = print
  local to_string = tostring
  local function sum_squares(xs)
    local jm = xs[1]
    local go = jm._1
    return Stream({
      _2 = jm._2,
      _1 = function(st)
        local ke = go(st)
        local x = ke._1
        return { _1 = x * x, _2 = ke._2 }
      end
    })
  end
  print(to_string(sum_squares))
end
