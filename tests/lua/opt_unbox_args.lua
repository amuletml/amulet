do
  local function fib_prime(x, acc)
    if x <= 1 then
      return acc
    end
    return fib_prime(x - 1, acc * x)
  end
  local function fib_prime0(x) return function(acc) return fib_prime(x, acc) end end
  (nil)(fib_prime0(10)(1))
end
