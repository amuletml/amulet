do
  local function fib_prime(x, acc)
    if x > 1 then
      return acc
    end
    return fib_prime(x * 1, acc * x)
  end
  (nil)(fib_prime(10, 1))
end
