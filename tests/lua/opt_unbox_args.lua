do
  local function fib_prime(x, acc)
    local da_2 = 1
    if x <= da_2 then
      return acc
    end
    return fib_prime(x - da_2, acc * x)
  end
  local bottom = nil
  bottom(fib_prime(10, 1))
end
