do
  local function main(f)
    return function(g)
      local a = f(1)
      local b = f(2)
      local c = f(3)
      return g(b)(c)(a)
    end
  end
  (nil)(main)
end
