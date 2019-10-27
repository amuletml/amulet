do
  local ignore = function(x)  end
  local function main(f)
    local tmp = f(nil)
    return { _1 = tmp._1, _2 = tmp._2 }
  end
  ignore(main)
end
