do
  local ignore = function(x)  end
  local function main(f)
    local x = f(nil)
    return { _1 = x._1, _2 = x._2 }
  end
  ignore(main)
end
