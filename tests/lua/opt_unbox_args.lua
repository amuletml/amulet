do
  local function fib_prime(x, acc)
    local db_2 = 1
    if x <= db_2 then
      return acc
    end
    return fib_prime(x - db_2, acc * x)
  end
  local bottom = nil
  (nil)(fib_prime(10, 1))
end
