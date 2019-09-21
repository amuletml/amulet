do
  local function main(f)
    return function(x)
      local a = f(1)
      if x.__tag == "None" then
        return f(a)
      end
      return f(a + x[1] * 2)
    end
  end
  (nil)(main)
end
