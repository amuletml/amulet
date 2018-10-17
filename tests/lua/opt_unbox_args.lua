do
  local function fib_prime(dw_1, dw_2)
    local db_2 = 1
    if dw_1 <= db_2 then
      return dw_2
    else
      return fib_prime(dw_1 - db_2, dw_2 * dw_1)
    end
  end
  local bottom = nil
  bottom(fib_prime(10, 1))
end
